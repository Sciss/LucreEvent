/*
 *  LinkedList.scala
 *  (LucreExpr)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package expr

import de.sciss.lucre.{event => evt}
import evt.EventLike
import stm.InMemory
import serial.DataInput
import impl.{LinkedListImpl => Impl}
import data.Iterator
import collection.immutable.{IndexedSeq => IIdxSeq}

object LinkedList {

  import expr.{Expr => Ex}

  final case class Update[S <: stm.Sys[S], Elem, U](list: LinkedList[S, Elem, U], changes: IIdxSeq[Change[S, Elem, U]])

  sealed trait Change[S <: stm.Sys[S], Elem, +U]

  sealed trait Collection[S <: stm.Sys[S], Elem] extends Change[S, Elem, Nothing] {
    def index: Int
    def elem: Elem
  }

  final case class Added[S <: stm.Sys[S], Elem](/* list: LinkedList[ S, Elem, U ], */ index: Int, elem: Elem)
    extends Collection[S, Elem]

  final case class Removed[S <: stm.Sys[S], Elem](/* list: LinkedList[ S, Elem, U ], */ index: Int, elem: Elem)
    extends Collection[S, Elem]

  final case class Element[S <: stm.Sys[S], Elem, U](elem: Elem, elemUpdate: U)
    extends Change[S, Elem, U]

  object Modifiable {
    /**
     * Returns a serializer for a modifiable list, given the provided mapping function from elements to their events.
     */
    def serializer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
      implicit elemSerializer: evt.Serializer[S, Elem]): serial.Serializer[S#Tx, S#Acc, Modifiable[S, Elem, U]] =
      Impl.activeModifiableSerializer(eventView)

    def read[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(in: DataInput, access: S#Acc)
                                      (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] =
      Impl.activeModifiableRead(eventView)(in, access)

    /**
     * Returns a serializer for a modifiable list of passive elements.
     */
    def serializer[S <: evt.Sys[S], Elem](implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): serial.Serializer[S#Tx, S#Acc, Modifiable[S, Elem, Unit]] =
      Impl.passiveModifiableSerializer

    def read[S <: evt.Sys[S], Elem](in: DataInput, access: S#Acc)
                                   (implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] =
      Impl.passiveModifiableRead(in, access)

    /**
     * Creates a new empty linked list, given the provided mapping function from elements to their events.
     */
    def apply[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                       (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] =
      Impl.newActiveModifiable[S, Elem, U](eventView)

    /**
     * Creates a new empty linked list for passive elements.
     */
    def apply[S <: evt.Sys[S], Elem](implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] =
      Impl.newPassiveModifiable[S, Elem]
  }

  /**
   * Modifiable extension of the linked list. Elements can be appended or prepended in O(1).
   * Removal of the head or last element is O(1). Arbitrary removal takes O(N).
   */
  trait Modifiable[S <: stm.Sys[S], Elem, U] extends LinkedList[S, Elem, U] {
    def addLast(elem: Elem)(implicit tx: S#Tx): Unit
    def addHead(elem: Elem)(implicit tx: S#Tx): Unit

    def removeLast()(implicit tx: S#Tx): Elem
    def removeHead()(implicit tx: S#Tx): Elem

    def insert  (index: Int, elem: Elem)(implicit tx: S#Tx): Unit
    def remove  (elem: Elem)(implicit tx: S#Tx): Boolean
    def removeAt(index: Int)(implicit tx: S#Tx): Elem

    def clear()(implicit tx: S#Tx): Unit
  }

  object Expr {
    type Modifiable[S <: stm.Sys[S], A] = LinkedList.Modifiable[S, Ex[S, A], evt.Change[A]]

    private val anyChanged: Ex[InMemory, Any] => EventLike[InMemory, evt.Change[Any]] = _.changed

    private def changed[S <: stm.Sys[S], A]: Ex[S, A] => EventLike[S, evt.Change[A]] =
      anyChanged.asInstanceOf[Ex[S, A] => EventLike[S, evt.Change[A]]]

    def serializer[S <: evt.Sys[S], A](implicit elemType: Type[A]): serial.Serializer[S#Tx, S#Acc, Expr[S, A]] =
      Impl.activeSerializer[S, Ex[S, A], evt.Change[A]](changed)(elemType.serializer[S])

    def read[S <: evt.Sys[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx, elemType: Type[A]): Expr[S, A] =
      Impl.activeRead[S, Ex[S, A], evt.Change[A]](changed)(in, access)(tx, elemType.serializer[S])

    object Modifiable {
      def serializer[S <: evt.Sys[S], A](implicit elemType: Type[A]): serial.Serializer[S#Tx, S#Acc, Expr.Modifiable[S, A]] =
        Impl.activeModifiableSerializer[S, Ex[S, A], evt.Change[A]](changed)(elemType.serializer[S])

      def read[S <: evt.Sys[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx, elemType: Type[A]): Expr.Modifiable[S, A] =
        Impl.activeModifiableRead[S, Ex[S, A], evt.Change[A]](changed)(in, access)(tx, elemType.serializer[S])

      def apply[S <: evt.Sys[S], A](implicit tx: S#Tx, peerType: Type[A]): Expr.Modifiable[S, A] =
        LinkedList.Modifiable[S, Ex[S, A], evt.Change[A]](changed)(tx, peerType.serializer[S])
    }
  }

  type Expr[S <: stm.Sys[S], A] = LinkedList[S, Ex[S, A], evt.Change[A]]

  def serializer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
    implicit elemSerializer: evt.Serializer[S, Elem]): serial.Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]] =
    Impl.activeSerializer[S, Elem, U](eventView)

  def read[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(in: DataInput, access: S#Acc)
                                    (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): LinkedList[S, Elem, U] =
    Impl.activeRead(eventView)(in, access)

  def serializer[S <: evt.Sys[S], Elem](implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): serial.Serializer[S#Tx, S#Acc, LinkedList[S, Elem, Unit]] =
    Impl.passiveSerializer[S, Elem]

  def read[S <: evt.Sys[S], Elem](in: DataInput, access: S#Acc)
                                 (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): LinkedList[S, Elem, Unit] =
    Impl.passiveRead(in, access)
}

/**
 * An observable linked list with fast `head` and `last` operations.
 * This is the read-only layer, see `LinkedList.Modifiable` for a mutable list.
 *
 * The list will report insertions and deletions, as well as forward designated
 * element events of type `U`.
 *
 * @tparam Elem      the element type of the list
 * @tparam U         the updates fired by the element type
 */
trait LinkedList[S <: stm.Sys[S], Elem, U] extends evt.Node[S] {
  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean
  def size    (implicit tx: S#Tx): Int

  def apply(index: Int)(implicit tx: S#Tx): Elem
  def get  (index: Int)(implicit tx: S#Tx): Option[Elem]

  def headOption(implicit tx: S#Tx): Option[Elem]
  def lastOption(implicit tx: S#Tx): Option[Elem]

  def head(implicit tx: S#Tx): Elem
  def last(implicit tx: S#Tx): Elem

  def iterator(implicit tx: S#Tx): Iterator[S#Tx, Elem]

  def modifiableOption: Option[LinkedList.Modifiable[S, Elem, U]]

  /**
    * Note: this is an O(n) operation.
    */
   def indexOf( elem: Elem )( implicit tx: S#Tx ) : Int

  //   def collectionChanged:  Event[     S, LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]]
  //   def elementChanged:     EventLike[ S, LinkedList.Element[    S, Elem, U ], LinkedList[ S, Elem, U ]]
  def changed: EventLike[S, LinkedList.Update[S, Elem, U]]

  def debugList()(implicit tx: S#Tx): List[Elem]
}

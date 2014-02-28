/*
 *  List.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package expr

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys}
import serial.{DataInput, Writable}
import impl.{ListImpl => Impl}
import data.Iterator
import collection.immutable.{IndexedSeq => Vec}
import stm.Disposable
import language.implicitConversions
import expr.{Boolean => _, Int => _}

object List {
  //  val Type: Type3[List] = Impl.TypeImpl
  //
  //  implicit def Ops[S <: Sys[S], Elem, U](list: List[S, Elem, U]): Ops[S, Elem] = new Impl.Ops(list)
  //
  //  trait Ops[S <: Sys[S], Elem] extends Any {
  //    def isEmpty_@   (implicit tx: S#Tx): Expr[S, Boolean]
  //    def nonEmpty_@  (implicit tx: S#Tx): Expr[S, Boolean]
  //    def size_@      (implicit tx: S#Tx): Expr[S, Int    ]
  //  }

  final case class Update[S <: Sys[S], Elem, U](list: List[S, Elem, U], changes: Vec[Change[S, Elem, U]])

  sealed trait Change[S <: Sys[S], Elem, +U]

  sealed trait Collection[S <: Sys[S], Elem] extends Change[S, Elem, Nothing] {
    def index: Int
    def elem: Elem
  }

  final case class Added[S <: Sys[S], Elem](index: Int, elem: Elem)
    extends Collection[S, Elem]

  final case class Removed[S <: Sys[S], Elem](index: Int, elem: Elem)
    extends Collection[S, Elem]

  final case class Element[S <: Sys[S], Elem, U](elem: Elem, elemUpdate: U)
    extends Change[S, Elem, U]

  object Modifiable {
    /** Returns a serializer for a modifiable list. */
    implicit def serializer[S <: Sys[S], Elem <: Publisher[S, U], U](
      implicit elemSerializer: evt.Serializer[S, Elem]): serial.Serializer[S#Tx, S#Acc, Modifiable[S, Elem, U]] =
      Impl.activeModifiableSerializer[S, Elem, U]

    def read[S <: Sys[S], Elem <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                                      (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] =
      Impl.activeModifiableRead(in, access)

    /** Returns a serializer for a modifiable list of passive elements. */
    implicit def serializer[S <: Sys[S], Elem](implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): serial.Serializer[S#Tx, S#Acc, Modifiable[S, Elem, Unit]] =
      Impl.passiveModifiableSerializer

    def read[S <: Sys[S], Elem](in: DataInput, access: S#Acc)
                                   (implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] =
      Impl.passiveModifiableRead(in, access)

    /** Creates a new empty linked list, given the provided mapping function from elements to their events. */
    def apply[S <: Sys[S], Elem <: Publisher[S, U], U](implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] =
      Impl.newActiveModifiable[S, Elem, U]

    /** Creates a new empty linked list for passive elements. */
    def apply[S <: Sys[S], Elem](implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] =
      Impl.newPassiveModifiable[S, Elem]
  }

  /** Modifiable extension of the linked list. Elements can be appended or prepended in O(1).
    * Removal of the head or last element is O(1). Arbitrary removal takes O(N).
    */
  trait Modifiable[S <: Sys[S], Elem, U] extends List[S, Elem, U] with evt.Node[S] {
    def addLast(elem: Elem)(implicit tx: S#Tx): Unit
    def addHead(elem: Elem)(implicit tx: S#Tx): Unit

    def removeLast()(implicit tx: S#Tx): Elem
    def removeHead()(implicit tx: S#Tx): Elem

    def insert  (index: Int, elem: Elem)(implicit tx: S#Tx): Unit
    def remove  (elem: Elem)(implicit tx: S#Tx): Boolean
    def removeAt(index: Int)(implicit tx: S#Tx): Elem

    def clear()(implicit tx: S#Tx): Unit
  }

  implicit def serializer[S <: Sys[S], Elem <: Publisher[S, U], U](
      implicit elemSerializer: evt.Serializer[S, Elem]): serial.Serializer[S#Tx, S#Acc, List[S, Elem, U]] =
    Impl.activeSerializer[S, Elem, U]

  def read[S <: Sys[S], Elem <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                                    (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): List[S, Elem, U] =
    Impl.activeRead(in, access)

  implicit def serializer[S <: Sys[S], Elem](implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): serial.Serializer[S#Tx, S#Acc, List[S, Elem, Unit]] =
    Impl.passiveSerializer[S, Elem]

  def passiveRead[S <: Sys[S], Elem](in: DataInput, access: S#Acc)
                             (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): List[S, Elem, Unit] =
    Impl.passiveRead(in, access)
}

/** An observable linked list with fast `head` and `last` operations.
  * This is the read-only layer, see `List.Modifiable` for a mutable list.
  *
  * The list will report insertions and deletions, as well as forward designated
  * element events of type `U`.
  *
  * @tparam Elem      the element type of the list
  * @tparam U         the updates fired by the element type
  */
trait List[S <: Sys[S], Elem, U] extends Writable with Disposable[S#Tx] with Publisher[S, List.Update[S, Elem, U]] {
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

  def modifiableOption: Option[List.Modifiable[S, Elem, U]]

  /** Note: this is an O(n) operation. */
  def indexOf(elem: Elem)(implicit tx: S#Tx): Int
}

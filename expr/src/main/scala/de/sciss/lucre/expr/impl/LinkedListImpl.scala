/*
 *  LinkedListImpl.scala
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
package impl

import serial.{DataInput, DataOutput, Serializer}
import lucre.{event => evt}
import evt.{Event, EventLike, NodeSerializer, impl => eimpl}
import data.Iterator
import scala.annotation.{tailrec, switch}
import collection.immutable.{IndexedSeq => Vec}
import collection.breakOut

object LinkedListImpl {
   import LinkedList.Modifiable

  // private def opNotSupported: Nothing = sys.error("Operation not supported")

  def newActiveModifiable[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                                   (implicit tx: S#Tx,
                                                    elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] = {

    new ActiveImpl(eventView) {
      protected val targets = evt.Targets[S]
      protected val sizeRef = tx.newIntVar(id, 0)
      protected val headRef = tx.newVar[C](id, null)(CellSer)
      protected val lastRef = tx.newVar[C](id, null)(CellSer)
    }
  }

  def newPassiveModifiable[S <: evt.Sys[S], Elem](implicit tx: S#Tx,
                                                  elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] = {

    new PassiveImpl[S, Elem] {
      protected val targets = evt.Targets[S]
      protected val sizeRef = tx.newIntVar(id, 0)
      protected val headRef = tx.newVar[C](id, null)(CellSer)
      protected val lastRef = tx.newVar[C](id, null)(CellSer)
    }
  }

  def activeSerializer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
    implicit elemSerializer: evt.Serializer[S, Elem]):
  NodeSerializer[S, LinkedList[S, Elem, U]] =
    new ActiveSer[S, Elem, U](eventView)

  def activeRead[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(in: DataInput, access: S#Acc)
                                          (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): LinkedList[S, Elem, U] = {
    val targets = evt.Targets.read(in, access)
    LinkedListImpl.activeRead(in, access, targets, eventView)
  }

  def passiveSerializer[S <: evt.Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]):
  NodeSerializer[S, LinkedList[S, Elem, Unit]] =
    new PassiveSer[S, Elem]

  def passiveRead[S <: evt.Sys[S], Elem](in: DataInput, access: S#Acc)
                                        (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): LinkedList[S, Elem, Unit] = {
    val targets = evt.Targets.read(in, access)
    LinkedListImpl.passiveRead(in, access, targets)
  }

  def activeModifiableSerializer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
    implicit elemSerializer: evt.Serializer[S, Elem]):
  NodeSerializer[S, Modifiable[S, Elem, U]] =
    new ActiveModSer[S, Elem, U](eventView)

  def activeModifiableRead[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(in: DataInput, access: S#Acc)
                                                    (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] = {
    val targets = evt.Targets.read(in, access)
    LinkedListImpl.activeRead(in, access, targets, eventView)
  }

  def passiveModifiableSerializer[S <: evt.Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]):
  NodeSerializer[S, Modifiable[S, Elem, Unit]] =
    new PassiveModSer[S, Elem]

  def passiveModifiableRead[S <: evt.Sys[S], Elem](in: DataInput, access: S#Acc)
                                                  (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] = {
    val targets = evt.Targets.read(in, access)
    LinkedListImpl.passiveRead(in, access, targets)
  }

  private class ActiveSer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                                   (implicit elemSerializer: evt.Serializer[S, Elem])
    extends NodeSerializer[S, LinkedList[S, Elem, U]] with evt.Reader[S, LinkedList[S, Elem, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): LinkedList[S, Elem, U] = {
      LinkedListImpl.activeRead(in, access, targets, eventView)
    }
  }

  private class PassiveSer[S <: evt.Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends NodeSerializer[S, LinkedList[S, Elem, Unit]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): LinkedList[S, Elem, Unit] = {
      LinkedListImpl.passiveRead(in, access, targets)
    }
  }

  private class ActiveModSer[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                                      (implicit elemSerializer: evt.Serializer[S, Elem])
    extends NodeSerializer[S, Modifiable[S, Elem, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Modifiable[S, Elem, U] = {
      LinkedListImpl.activeRead(in, access, targets, eventView)
    }
  }

  private class PassiveModSer[S <: evt.Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends NodeSerializer[S, Modifiable[S, Elem, Unit]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Modifiable[S, Elem, Unit] = {
      LinkedListImpl.passiveRead(in, access, targets)
    }
  }

  private def activeRead[S <: evt.Sys[S], Elem, U](in: DataInput, access: S#Acc, _targets: evt.Targets[S],
                                                   eventView: Elem => EventLike[S, U])
                                                  (implicit tx: S#Tx,
                                                   elemSerializer: evt.Serializer[S, Elem]): Impl[S, Elem, U] =
    new ActiveImpl(eventView) {
      protected val targets = _targets
      protected val sizeRef = tx.readIntVar(id, in)
      protected val headRef = tx.readVar[C](id, in)
      protected val lastRef = tx.readVar[C](id, in)
    }

  private def passiveRead[S <: evt.Sys[S], Elem](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                                (implicit tx: S#Tx,
                                                 elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Impl[S, Elem, Unit] =
    new PassiveImpl[S, Elem] {
      protected val targets = _targets
      protected val sizeRef = tx.readIntVar(id, in)
      protected val headRef = tx.readVar[C](id, in)
      protected val lastRef = tx.readVar[C](id, in)
    }

  private final class Cell[S <: stm.Sys[S], Elem](val elem: Elem,
                                                  val pred: S#Var[Cell[S, Elem]], val succ: S#Var[Cell[S, Elem]])

  private final class Iter[S <: stm.Sys[S], Elem](private var cell: Cell[S, Elem]) extends Iterator[S#Tx, Elem] {
    override def toString = if (cell == null) "empty iterator" else "non-empty iterator"

    def hasNext(implicit tx: S#Tx) = cell != null

    def next()(implicit tx: S#Tx): Elem = {
      if (cell == null) throw new NoSuchElementException("next on empty iterator")
      val res = cell.elem
      cell    = cell.succ()
      res
    }
  }

  private abstract class ActiveImpl[S <: evt.Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
    implicit protected val elemSerializer: evt.Serializer[S, Elem])
    extends Impl[S, Elem, U] {
    list =>

    final protected def elementChanged: EventLike[S, LinkedList.Update[S, Elem, U]] =
      ElementEvent

    final protected def registerElement(elem: Elem)(implicit tx: S#Tx): Unit =
      eventView(elem) ---> ElementEvent

    final protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit =
      eventView(elem) -/-> ElementEvent

    private object ElementEvent
      extends eimpl.EventImpl[S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]]
      with evt.InvariantEvent[S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]] {

      protected def reader: evt.Reader[S, LinkedList[S, Elem, U]] = activeSerializer(eventView)

      final val slot = 1

      def node: LinkedList[S, Elem, U] = list

      def connect   ()(implicit tx: S#Tx): Unit = foreach(registerElement  )
      def disconnect()(implicit tx: S#Tx): Unit = foreach(unregisterElement)

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[LinkedList.Update[S, Elem, U]] = {
        val changes: Vec[LinkedList.Element[S, Elem, U]] = pull.parents(this).flatMap(sel => {
          val evt = sel.devirtualize[U, Elem](elemSerializer)
          val opt: Option[LinkedList.Element[S, Elem, U]] = pull(evt).map(LinkedList.Element(evt.node, _)) // u => LinkedList.Element( list, elem, u ))
          opt
        })(breakOut)

        if (changes.isEmpty) None else Some(LinkedList.Update(list, changes))
      }
    }

    final protected def reader: evt.Reader[S, LinkedList[S, Elem, U]] = activeSerializer(eventView)

    final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent    .slot => ChangeEvent
      case CollectionEvent.slot => CollectionEvent
      case ElementEvent   .slot => ElementEvent
    }
  }

  private abstract class PassiveImpl[S <: evt.Sys[S], Elem](implicit protected val elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends Impl[S, Elem, Unit] {
    // Dummy.apply is a cheap method now
    final protected def elementChanged: EventLike[S, LinkedList.Update[S, Elem, Unit]] =
      evt.Dummy.apply

    final protected def registerElement  (elem: Elem)(implicit tx: S#Tx) = ()
    final protected def unregisterElement(elem: Elem)(implicit tx: S#Tx) = ()

    final protected def reader: evt.Reader[S, LinkedList[S, Elem, Unit]] = passiveSerializer

    final def select(slot: Int /* , invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent    .slot => ChangeEvent
      case CollectionEvent.slot => CollectionEvent
    }
  }

  private abstract class Impl[S <: evt.Sys[S], Elem, U] extends Modifiable[S, Elem, U] {
    list =>

    final protected type C = Cell[S, Elem]

    protected def headRef: S#Var[C]
    protected def lastRef: S#Var[C]
    protected def sizeRef: S#Var[Int]

    implicit protected def elemSerializer: Serializer[S#Tx, S#Acc, Elem]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit

    override def toString = "LinkedList" + id

    // ---- event behaviour ----

    protected implicit object CellSer extends Serializer[S#Tx, S#Acc, C] {
      def write(cell: C, out: DataOutput): Unit =
        if (cell != null) {
          out.writeByte(1)
          elemSerializer.write(cell.elem, out)
          cell.pred.write(out)
          cell.succ.write(out)
        } else {
          out.writeByte(0)
        }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): C = {
        (in.readByte: @switch) match {
          case 1 =>
            val elem = elemSerializer.read(in, access)
            val pred = tx.readVar[C](id, in)
            val succ = tx.readVar[C](id, in)
            new Cell(elem, pred, succ)
          case 0 => null
          case cookie => sys.error("Unexpected cookie " + cookie)
        }
      }
    }

    protected def reader: evt.Reader[S, LinkedList[S, Elem, U]]

    protected object CollectionEvent
      extends eimpl.TriggerImpl[S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]]
      with eimpl.EventImpl     [S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]]
      with evt.InvariantEvent  [S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]]
      with eimpl.Root          [S, LinkedList.Update[S, Elem, U]] {
      protected def reader = list.reader

      final val slot = 0

      def node: LinkedList[S, Elem, U] = list
    }

    protected object ChangeEvent
      extends evt.impl.EventImpl[S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]]
      with evt.InvariantEvent   [S, LinkedList.Update[S, Elem, U], LinkedList[S, Elem, U]] {

      protected def reader: evt.Reader[S, LinkedList[S, Elem, U]] = list.reader

      final val slot = 2

      def node: LinkedList[S, Elem, U] = list

      def connect   ()(implicit tx: S#Tx): Unit = {
        CollectionEvent ---> this
        elementChanged  ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        CollectionEvent -/-> this
        elementChanged  -/-> this
      }

      //      def --->(r: evt.Selector[S])(implicit tx: S#Tx) = ()
      //
      //      def -/->(r: evt.Selector[S])(implicit tx: S#Tx) = ()

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[LinkedList.Update[S, Elem, U]] = {
        val collOpt = if (pull.contains(CollectionEvent)) pull(CollectionEvent) else None
        val elemOpt = if (pull.contains(elementChanged)) pull(elementChanged) else None

        (collOpt, elemOpt) match {
          case (Some(_), None) => collOpt
          case (None, Some(_)) => elemOpt
          case (Some(LinkedList.Update(_, coll)), Some(LinkedList.Update(_, elem))) =>
            Some(LinkedList.Update(list, coll ++ elem))
          case _ => None
        }
      }

      //      def react[A1 >: LinkedList.Update[S, Elem, U]](fun: A1 => Unit)
      //                                                    (implicit tx: S#Tx): evt.Observer[S, A1, LinkedList[S, Elem, U]] =
      //        reactTx((_: S#Tx) => fun)
      //
      //      def reactTx[A1 >: LinkedList.Update[S, Elem, U]](fun: S#Tx => A1 => Unit)
      //                                                      (implicit tx: S#Tx): evt.Observer[S, A1, LinkedList[S, Elem, U]] = {
      //        val obs = evt.Observer[S, A1, LinkedList[S, Elem, U]](list.reader, fun)
      //        obs.add(CollectionEvent)
      //        obs.add(elementChanged)
      //        obs
      //      }

      // def isSource( pull: evt.Pull[ S ]) : Boolean = opNotSupported
    }

    //      final /* private[event] */ def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = (slot: @switch) match {
    //         case 1 => CollectionEvent
    //         case 2 => elementChanged
    //      }

    def modifiableOption: Option[LinkedList.Modifiable[S, Elem, U]] = Some(this)

    final def indexOf(elem: Elem)(implicit tx: S#Tx): Int = {
      var idx = 0
      var rec = headRef()
      while (rec != null) {
        if (rec.elem == elem) return idx
        idx += 1
        rec = rec.succ()
      }
      -1
    }

    final def apply(idx: Int)(implicit tx: S#Tx): Elem =
      get(idx).getOrElse(throw new IndexOutOfBoundsException(idx.toString))

    final def get(idx: Int)(implicit tx: S#Tx): Option[Elem] = {
      if (idx < 0) return None
      var left = idx
      var rec = headRef()
      while (rec != null && left > 0) {
        left -= 1
        rec = rec.succ()
      }
      if (rec == null) None else Some(rec.elem)
    }

    final def addLast(elem: Elem)(implicit tx: S#Tx): Unit = {
      val pred      = lastRef()
      val succ      = null
      val idx       = sizeRef()
      insert(elem, pred, succ, idx)
    }

    final def addHead(elem: Elem)(implicit tx: S#Tx): Unit = {
      val pred      = null
      val succ      = headRef()
      val idx       = 0
      insert(elem, pred, succ, idx)
    }

    def insert(index: Int, elem: Elem)(implicit tx: S#Tx): Unit = {
      if (index < 0)      throw new IndexOutOfBoundsException(index.toString)
      var pred      = null: C
      var succ      = headRef()
      var idx       = 0
      while (idx < index) {
        if (succ == null) throw new IndexOutOfBoundsException(index.toString)
        pred  = succ
        succ  = succ.succ()
        idx  += 1
      }
      insert(elem, pred, succ, idx)
    }

    private def insert(elem: Elem, pred: C, succ: C, idx: Int)(implicit tx: S#Tx): Unit = {
      val recPred   = tx.newVar[C](id, pred)
      val recSucc   = tx.newVar[C](id, succ)
      val rec       = new Cell(elem, recPred, recSucc)
      val predSucc  = if (pred == null) headRef else pred.succ
      val succPred  = if (succ == null) lastRef else succ.pred
      predSucc()    = rec
      succPred()    = rec
      sizeRef.transform(_ + 1)
      if (isConnected) {
        registerElement(elem)
        fireAdded(idx, elem)
      }
    }

    final protected def isConnected(implicit tx: S#Tx) = targets.nonEmpty

    final protected def foreach(fun: Elem => Unit)(implicit tx: S#Tx): Unit = {
      @tailrec def loop(cell: C): Unit =
        if (cell != null) {
          fun(cell.elem)
          loop(cell.succ())
        }

      loop(headRef())
    }

    private def fireAdded(idx: Int, elem: Elem)(implicit tx: S#Tx): Unit =
      CollectionEvent(LinkedList.Update(list, Vec(LinkedList.Added(idx, elem))))

    private def fireRemoved(idx: Int, elem: Elem)(implicit tx: S#Tx): Unit =
      CollectionEvent(LinkedList.Update(list, Vec(LinkedList.Removed(idx, elem))))

    final def remove(elem: Elem)(implicit tx: S#Tx): Boolean = {
      var rec = headRef()
      var idx = 0
      while (rec != null) {
        if (rec.elem == elem) {
          removeCell(rec)
          if (isConnected) {
            fireRemoved(idx, elem)
          }
          return true
        }
        rec = rec.succ()
        idx += 1
      }
      false
    }

    final def removeAt(index: Int)(implicit tx: S#Tx): Elem = {
      if (index < 0) throw new IndexOutOfBoundsException(index.toString)
      var rec = headRef()
      if (rec == null) throw new IndexOutOfBoundsException(index.toString)
      var idx = 0
      while (idx < index) {
        rec = rec.succ()
        if (rec == null) throw new IndexOutOfBoundsException(index.toString)
        idx += 1
      }

      val e = rec.elem
      removeCell(rec)
      if (isConnected) {
        fireRemoved(idx, e)
      }
      e
    }

    // unlinks a cell and disposes it. does not fire. decrements sizeRef
    private def removeCell(cell: C)(implicit tx: S#Tx): Unit = {
      val pred = cell.pred()
      val succ = cell.succ()
      if (pred != null) {
        pred.succ() = succ
      } else {
        headRef() = succ
      }
      if (succ != null) {
        succ.pred() = pred
      } else {
        lastRef() = pred
      }
      sizeRef.transform(_ - 1)
      disposeCell(cell)
    }

    final def removeLast()(implicit tx: S#Tx): Elem = {
      val rec = lastRef()
      if (rec == null) throw new NoSuchElementException("last of empty list")

      val pred  = rec.pred()
      val e     = rec.elem
      val idx   = sizeRef() - 1
      disposeCell(rec)
      sizeRef() = idx
      lastRef() = pred
      if (pred == null) {
        headRef() = null
      } else {
        pred.succ() = null
      }
      if (isConnected) {
        fireRemoved(idx, e)
      }
      e
    }

    final def removeHead()(implicit tx: S#Tx): Elem = {
      val rec = headRef()
      if (rec == null) throw new NoSuchElementException("head of empty list")

      val succ = rec.succ()
      val e = rec.elem
      disposeCell(rec)
      sizeRef.transform(_ - 1)
      headRef() = succ
      if (succ == null) {
        lastRef() = null
      } else {
        succ.pred() = null
      }
      if (isConnected) {
        fireRemoved(0, e)
      }
      e
    }

    final def clear()(implicit tx: S#Tx): Unit =
      while (nonEmpty) removeLast()

    // unregisters element event. disposes cell contents, but does not unlink, nor fire.
    private def disposeCell(cell: C)(implicit tx: S#Tx): Unit = {
      unregisterElement(cell.elem)
      cell.pred.dispose()
      cell.succ.dispose()
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      var rec = headRef()
      while (rec != null) {
        val tmp = rec.succ()
        disposeCell(rec)
        rec = tmp
      }
      sizeRef.dispose()
      headRef.dispose()
      lastRef.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      sizeRef.write(out)
      headRef.write(out)
      lastRef.write(out)
    }

    final def isEmpty (implicit tx: S#Tx): Boolean = size == 0
    final def nonEmpty(implicit tx: S#Tx): Boolean = size > 0
    final def size    (implicit tx: S#Tx): Int     = sizeRef()

    final def headOption(implicit tx: S#Tx): Option[Elem] = {
      val rec = headRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def lastOption(implicit tx: S#Tx): Option[Elem] = {
      val rec = lastRef()
      if (rec != null) Some(rec.elem) else None
    }

    final def head(implicit tx: S#Tx): Elem = {
      val rec = headRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("head of empty list")
    }

    final def last(implicit tx: S#Tx): Elem = {
      val rec = lastRef()
      if (rec != null) rec.elem else throw new NoSuchElementException("last of empty list")
    }

    final def iterator(implicit tx: S#Tx): Iterator[S#Tx, Elem] = new Iter(headRef())

    protected def elementChanged: EventLike[S, LinkedList.Update[S, Elem, U]]
    final def changed           : EventLike[S, LinkedList.Update[S, Elem, U]] = ChangeEvent

    final def debugList()(implicit tx: S#Tx): List[Elem] = iterator.toList
  }
}
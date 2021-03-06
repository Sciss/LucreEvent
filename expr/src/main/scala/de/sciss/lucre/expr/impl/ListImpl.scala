/*
 *  ListImpl.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package expr
package impl

import de.sciss.lucre.data.Iterator
import de.sciss.lucre.event.{impl => eimpl, _}
import de.sciss.lucre.expr.{Boolean => _Boolean, Int => _Int, String => _String}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.{switch, tailrec}
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object ListImpl {
  import de.sciss.lucre.expr.List.Modifiable

  //  object TypeImpl extends TypeImpl3[List] {
  //    final val typeID = 0x10004
  //  }
  //
  //  final class Ops[S <: Sys[S], Elem, U](val `this`: List[S, Elem, U]) extends AnyVal with List.Ops[S, Elem] {
  //    def size_@    (implicit tx: S#Tx): Expr[S, Int    ] = ???
  //    def nonEmpty_@(implicit tx: S#Tx): Expr[S, Boolean] = ???
  //    def isEmpty_@ (implicit tx: S#Tx): Expr[S, Boolean] = ???
  //  }
  //
  //  private final val SizeID  = 0
  //
  //  private final class SizeExpr[S <: Sys[S]](protected val targets: evt.Targets[S], list: List[S, _, _])
  //    extends NodeImpl[S, Int] {
  //
  //    def value(implicit tx: S#Tx): Int = list.size
  //
  //    protected def writeData(out: DataOutput): Unit = {
  //      out.writeByte(1)  // 'op'
  //      out.writeInt(SizeID)
  //      list.write(out)
  //    }
  //
  //    // Hitting a brick wall. While we might still get the `elemSerializer` from `list`,
  //    // this does not hold for the (not yet written) `Type.Extension3`.
  //    // On the other hand... At least with these expressions, the elements do not
  //    // need to be deserialized ever, so we can get away with a dummy serializer?
  //    // - answer: NO! because we might get a List.Element event, and we can't filter that
  //    //   out in advantage, so that would throw up on the invalid element serializer...
  //    protected def reader: Reader[S, Expr[S, Int]] = Int.serializer
  //
  //    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[Int]] = ???
  //
  //    def connect   ()(implicit tx: S#Tx): Unit = list.changed ---> this
  //    def disconnect()(implicit tx: S#Tx): Unit = list.changed -/-> this
  //  }

  // private final val SER_VERSION = 0

  def newActiveModifiable[S <: Sys[S], Elem <: Publisher[S, U], U](
                              implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] = {
    new ActiveImpl[S, Elem, U] {
      protected val targets = evt.Targets[S]
      protected val sizeRef = tx.newIntVar(id, 0)
      protected val headRef = tx.newVar[C](id, null)(CellSer)
      protected val lastRef = tx.newVar[C](id, null)(CellSer)
    }
  }

  def newPassiveModifiable[S <: Sys[S], Elem](
                     implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] = {

    new PassiveImpl[S, Elem] {
      protected val targets = evt.Targets[S]
      protected val sizeRef = tx.newIntVar(id, 0)
      protected val headRef = tx.newVar[C](id, null)(CellSer)
      protected val lastRef = tx.newVar[C](id, null)(CellSer)
    }
  }

  def activeSerializer[S <: Sys[S], Elem <: Publisher[S, U], U](
      implicit elemSerializer: evt.Serializer[S, Elem]): NodeSerializer[S, List[S, Elem, U]] =
    new ActiveSer[S, Elem, U]

  def activeRead[S <: Sys[S], Elem <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                                    (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): List[S, Elem, U] = {
    val targets = evt.Targets.read(in, access)
    //    val version = in.readByte()
    //    require(version == SER_VERSION, s"Unexpected version (required $SER_VERSION, found $version)")
    val opID = in.readInt()
    if (opID == 0) ListImpl.activeRead(in, access, targets)
    else sys.error(s"Expected opID 0 -- found $opID")
  }

  def passiveSerializer[S <: Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]):
  NodeSerializer[S, List[S, Elem, Unit]] =
    new PassiveSer[S, Elem]

  def passiveRead[S <: Sys[S], Elem](in: DataInput, access: S#Acc)
                          (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): List[S, Elem, Unit] = {
    val targets = evt.Targets.read(in, access)
    ListImpl.passiveRead(in, access, targets)
  }

  def activeModifiableSerializer[S <: Sys[S], Elem <: Publisher[S, U], U](
    implicit elemSerializer: evt.Serializer[S, Elem]):
  NodeSerializer[S, Modifiable[S, Elem, U]] = new ActiveModSer[S, Elem, U]

  def activeModifiableRead[S <: Sys[S], Elem <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                              (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Modifiable[S, Elem, U] = {
    val targets = evt.Targets.read(in, access)
    ListImpl.activeRead(in, access, targets)
  }

  def passiveModifiableSerializer[S <: Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]):
  NodeSerializer[S, Modifiable[S, Elem, Unit]] =
    new PassiveModSer[S, Elem]

  def passiveModifiableRead[S <: Sys[S], Elem](in: DataInput, access: S#Acc)
                      (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, Unit] = {
    val targets = evt.Targets.read(in, access)
    ListImpl.passiveRead(in, access, targets)
  }

  private class ActiveSer[S <: Sys[S], Elem <: Publisher[S, U], U](implicit elemSerializer: evt.Serializer[S, Elem])
    extends NodeSerializer[S, List[S, Elem, U]] with evt.Reader[S, List[S, Elem, U]] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): List[S, Elem, U] with evt.Node[S] = ListImpl.activeRead(in, access, targets)
  }

  private class PassiveSer[S <: Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends NodeSerializer[S, List[S, Elem, Unit]] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): List[S, Elem, Unit] with evt.Node[S] = ListImpl.passiveRead(in, access, targets)
  }

  private class ActiveModSer[S <: Sys[S], Elem <: Publisher[S, U], U](implicit elemSerializer: evt.Serializer[S, Elem])
    extends NodeSerializer[S, Modifiable[S, Elem, U]] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Modifiable[S, Elem, U] = {
      ListImpl.activeRead(in, access, targets)
    }
  }

  private class PassiveModSer[S <: Sys[S], Elem](implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends NodeSerializer[S, Modifiable[S, Elem, Unit]] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Modifiable[S, Elem, Unit] = {
      ListImpl.passiveRead(in, access, targets)
    }
  }

  private def activeRead[S <: Sys[S], Elem <: Publisher[S, U], U](in: DataInput, access: S#Acc,
                                                                  _targets: evt.Targets[S])
                                       (implicit tx: S#Tx, elemSerializer: evt.Serializer[S, Elem]): Impl[S, Elem, U] =
    new ActiveImpl[S, Elem, U] {
      protected val targets = _targets
      protected val sizeRef = tx.readIntVar(id, in)
      protected val headRef = tx.readVar[C](id, in)
      protected val lastRef = tx.readVar[C](id, in)
    }

  private def passiveRead[S <: Sys[S], Elem](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                            (implicit tx: S#Tx,
                                             elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Impl[S, Elem, Unit] =
    new PassiveImpl[S, Elem] {
      protected val targets = _targets
      protected val sizeRef = tx.readIntVar(id, in)
      protected val headRef = tx.readVar[C](id, in)
      protected val lastRef = tx.readVar[C](id, in)
    }

  private final class Cell[S <: Sys[S], Elem](val elem: Elem,
                                              val pred: S#Var[Cell[S, Elem]], val succ: S#Var[Cell[S, Elem]])

  private final class Iter[S <: Sys[S], Elem](private var cell: Cell[S, Elem]) extends Iterator[S#Tx, Elem] {
    override def toString = if (cell == null) "empty iterator" else "non-empty iterator"

    def hasNext(implicit tx: S#Tx) = cell != null

    def next()(implicit tx: S#Tx): Elem = {
      if (cell == null) throw new NoSuchElementException("next on empty iterator")
      val res = cell.elem
      cell    = cell.succ()
      res
    }
  }

  private abstract class ActiveImpl[S <: Sys[S], Elem <: Publisher[S, U], U](
    implicit protected val elemSerializer: evt.Serializer[S, Elem])
    extends Impl[S, Elem, U] {

    list =>

    final protected def registerElement(elem: Elem)(implicit tx: S#Tx): Unit =
      elem.changed ---> elementChanged

    final protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit =
      elem.changed -/-> elementChanged

    protected object elementChanged
      extends eimpl.EventImpl[S, List.Update[S, Elem, U], List[S, Elem, U]]
      with evt.InvariantEvent[S, List.Update[S, Elem, U], List[S, Elem, U]] {

      protected def reader: evt.Reader[S, List[S, Elem, U]] = activeSerializer[S, Elem, U]

      final val slot = 1

      def node /* : List[S, Elem, U] */ = list

      def connect   ()(implicit tx: S#Tx): Unit = foreach(registerElement  )
      def disconnect()(implicit tx: S#Tx): Unit = foreach(unregisterElement)

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[List.Update[S, Elem, U]] = {
        val changes: Vec[List.Element[S, Elem, U]] = pull.parents(this).flatMap { sel =>
          val evt = sel.devirtualize[U, Elem](elemSerializer)
          val opt: Option[List.Element[S, Elem, U]] = pull(evt).map(List.Element(evt.node, _)) // u => List.Element( list, elem, u ))
          opt
        } (breakOut)

        if (changes.isEmpty) None else Some(List.Update(list, changes))
      }
    }

    final protected def reader: evt.Reader[S, List[S, Elem, U]] = activeSerializer[S, Elem, U]

    final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case `changed`        .slot => changed
      case CollectionEvent  .slot => CollectionEvent
      case `elementChanged` .slot => elementChanged
    }
  }

  private abstract class PassiveImpl[S <: Sys[S], Elem](implicit protected val elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends Impl[S, Elem, Unit] {
    // Dummy.apply is a cheap method now
    final protected def elementChanged: EventLike[S, List.Update[S, Elem, Unit]] =
      evt.Dummy.apply

    final protected def registerElement  (elem: Elem)(implicit tx: S#Tx) = ()
    final protected def unregisterElement(elem: Elem)(implicit tx: S#Tx) = ()

    final protected def reader: evt.Reader[S, List[S, Elem, Unit]] = passiveSerializer

    final def select(slot: Int /* , invariant: Boolean */): Event[S, Any, Any] = slot match {
      case `changed`      .slot => changed
      case CollectionEvent.slot => CollectionEvent
    }
  }

  private abstract class Impl[S <: Sys[S], Elem, U] extends Modifiable[S, Elem, U] {
    list =>

    final protected type C = Cell[S, Elem]

    protected def headRef: S#Var[C]
    protected def lastRef: S#Var[C]
    protected def sizeRef: S#Var[Int]

    implicit protected def elemSerializer: Serializer[S#Tx, S#Acc, Elem]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit

    override def toString = "List" + id

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

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): C =
        (in.readByte: @switch) match {
          case 1 =>
            val elem = elemSerializer.read(in, access)
            val pred = tx.readVar[C](id, in)
            val succ = tx.readVar[C](id, in)
            new Cell[S, Elem](elem, pred, succ)
          case 0 => null
          case cookie => sys.error(s"Unexpected cookie $cookie")
        }
    }

    protected def reader: evt.Reader[S, List[S, Elem, U]]

    protected object CollectionEvent
      extends eimpl.TriggerImpl[S, List.Update[S, Elem, U], List[S, Elem, U]]
      with eimpl.EventImpl     [S, List.Update[S, Elem, U], List[S, Elem, U]]
      with evt.InvariantEvent  [S, List.Update[S, Elem, U], List[S, Elem, U]]
      with eimpl.Root          [S, List.Update[S, Elem, U]] {
      protected def reader = list.reader

      final val slot = 0

      def node /* : List[S, Elem, U] */ = list
    }

    object changed
      extends evt.impl.EventImpl[S, List.Update[S, Elem, U], List[S, Elem, U]]
      with evt.InvariantEvent   [S, List.Update[S, Elem, U], List[S, Elem, U]] {

      protected def reader: evt.Reader[S, List[S, Elem, U]] = list.reader

      final val slot = 2

      def node /* : List[S, Elem, U] */ = list

      def connect   ()(implicit tx: S#Tx): Unit = {
        CollectionEvent ---> this
        elementChanged  ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        CollectionEvent -/-> this
        elementChanged  -/-> this
      }

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[List.Update[S, Elem, U]] = {
        val collOpt = if (pull.contains(CollectionEvent)) pull(CollectionEvent) else None
        val elemOpt = if (pull.contains(elementChanged)) pull(elementChanged) else None

        (collOpt, elemOpt) match {
          case (Some(_), None) => collOpt
          case (None, Some(_)) => elemOpt
          case (Some(List.Update(_, coll)), Some(List.Update(_, elem))) =>
            Some(List.Update(list, coll ++ elem))
          case _ => None
        }
      }
    }

    def modifiableOption: Option[List.Modifiable[S, Elem, U]] = Some(this)

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
      val rec       = new Cell[S, Elem](elem, recPred, recSucc)
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
      CollectionEvent(List.Update(list, Vec(List.Added(idx, elem))))

    private def fireRemoved(idx: Int, elem: Elem)(implicit tx: S#Tx): Unit =
      CollectionEvent(List.Update(list, Vec(List.Removed(idx, elem))))

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

    // deregisters element event. disposes cell contents, but does not unlink, nor fire.
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

    protected def elementChanged: EventLike[S, List.Update[S, Elem, U]]

    // final def debugList()(implicit tx: S#Tx): scala.List[Elem] = iterator.toList
  }
}
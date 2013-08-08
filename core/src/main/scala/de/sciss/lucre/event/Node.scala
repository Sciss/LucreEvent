/*
 *  Node.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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
package event

import collection.immutable.{IndexedSeq => IIdxSeq}
import stm.Mutable
import annotation.switch
import de.sciss.serial.{Writable, DataInput, DataOutput}

/**
 * An abstract trait uniting invariant and mutating readers.
 */
trait Reader[S <: stm.Sys[S], +Repr] {
  def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Repr with Node[S]
}

trait NodeSerializer[S <: Sys[S], Repr <: Writable]
  extends Reader[S, Repr] with serial.Serializer[S#Tx, S#Acc, Repr] {

  final def write(v: Repr, out: DataOutput): Unit = v.write(out)

  final def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr = {
    val targets = Targets.read[S](in, access)
    read(in, access, targets)
  }
}

object Targets {
  //   private type I = InMemory
  //
  //   private implicit def childrenSer[ S <: Sys[ S ]] : stm.Serializer[ S#Tx, S#Acc, Children[ S ]] =
  //      anyChildrenSer.asInstanceOf[ stm.Serializer[ S#Tx, S#Acc, Children[ S ]]]
  //
  //   private val anyChildrenSer = stm.Serializer.indexedSeq[ I#Tx, I#Acc, (Int, Selector[ I ])](
  //      stm.Serializer.tuple2[ I#Tx, I#Acc, Int, Selector[ I ]](
  //         stm.Serializer.Int, Selector.serializer[ I ]
  //      )
  //   )

  private implicit def childrenSerializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Children[S]] =
    anyChildrenSer.asInstanceOf[ChildrenSer[S]]

  private val anyChildrenSer = new ChildrenSer[InMemory]

  private final class ChildrenSer[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, Children[S]] {
    def write(v: Children[S], out: DataOutput): Unit = {
      out.writeInt(v.size)
      v.foreach { tup =>
        out.writeByte(tup._1)
        tup._2.writeSelector(out) // same as Selector.serializer.write(tup._2)
      }
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Children[S] = {
      val sz = in.readInt()
      if (sz == 0) Vector.empty else Vector.fill(sz) {
        val slot      = in.readByte()
        val selector  = Selector.read(in, access)
        (slot, selector)
      }
    }
  }

  def apply[S <: Sys[S]](implicit tx: S#Tx): Targets[S] = {
    val id        = tx.newID()
    val children  = tx.newEventVar[Children[S]](id)
    val invalid   = tx.newEventValidity(id)
    new Impl(0, id, children, invalid)
  }

  def partial[S <: Sys[S]](implicit tx: S#Tx): Targets[S] = {
    apply[S]
    //      val id         = tx.newPartialID()
    //      val children   = tx.newPartialVar[ Children[ S ]]( id, NoChildren )
    //      val invalid    = tx.newIntVar( id, 0 ) // XXX should this be removed? or partial?
    //      new Impl( 1, id, children, invalid )
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Targets[S] = {
    (in.readByte(): @switch) match {
      case 0      => readIdentified(in, access)
      case 1      => readIdentifiedPartial(in, access)
      case cookie => sys.error("Unexpected cookie " + cookie)
    }
  }

  private[event] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Targets[S] = {
    val id = tx.readID(in, access)
    val children = tx.readEventVar[Children[S]](id, in)
    val valid    = tx.readEventValidity(id, in)
    new Impl[S](0, id, children, valid)
  }

  private[event] def readIdentifiedPartial[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Targets[S] = {
    readIdentified(in, access)
  }

  private final class Impl[S <: Sys[S]](cookie: Int, val id: S#ID, childrenVar: event.Var[S, Children[S]],
                                        valid: Validity[S#Tx])
    extends Targets[S] {

    def write(out: DataOutput): Unit = {
      out        .writeByte(cookie)
      id         .write(out)
      childrenVar.write(out)
      valid      .write(out)
    }

    private[lucre] def isPartial : Boolean = cookie == 1

    def dispose()(implicit tx: S#Tx): Unit = {
      require( children.isEmpty, "Disposing a event reactor which is still being observed" )
      id         .dispose()
      childrenVar.dispose()
      valid      .dispose()
    }

    private[event] def children(implicit tx: S#Tx): Children[S] = childrenVar.getOrElse(NoChildren)

    override def toString = "Targets" + id

    private[event] def add(slot: Int, sel: /* MMM Expanded */ Selector[S])(implicit tx: S#Tx): Boolean = {
      log(s"$this.add($slot, $sel)")
      val tup = (slot.toByte, sel)
      val old = childrenVar.get // .getFresh
      log(s"$this - old children = $old")
      old match {
        case Some(seq) =>
          childrenVar() = seq :+ tup
          !seq.exists(_._1 == slot)
        case _ =>
          childrenVar() = Vector(tup)
          true
      }
    }

    private[event] def resetAndValidate(slot: Int, sel: Selector[S])(implicit tx: S#Tx): Unit = {
      log(s"$this.resetAndValidate($slot, $sel)")
      val tup = (slot.toByte, sel)
      // I'm not sure this is correct... Why would we only read children if this was partial?
      // When this is called (from InvariantEvent.--->)
      val old = /* if (isPartial) */ childrenVar.getOrElse(NoChildren[S]) /* else NoChildren[S] */
      childrenVar() = old :+ tup
      validated() // (slot)
    }

    private[event] def remove(slot: Int, sel: Selector[S])(implicit tx: S#Tx): Boolean = {
      log(s"$this.remove($slot, $sel)")
      val tup = (slot, sel)
      val xs = childrenVar.getOrElse(NoChildren)
      log(s"$this - old children = $xs")
      val i = xs.indexOf(tup)
      if (i >= 0) {
        val xs1 = xs.patch(i, Vector.empty, 1) // XXX crappy way of removing a single element
        childrenVar() = xs1
        !xs1.exists(_._1 == slot)
      } else {
        log(s"$this - selector not found")
        false
      }
    }

    private[event] def observers(implicit tx: S#Tx): IIdxSeq[ObserverKey[S]] =
      children.flatMap(_._2.toObserverKey)

    def isEmpty (implicit tx: S#Tx): Boolean = children.isEmpty   // XXX TODO this is expensive
    def nonEmpty(implicit tx: S#Tx): Boolean = children.nonEmpty  // XXX TODO this is expensive

    private[event] def _targets: Targets[S] = this

    private[event] def isInvalid(implicit tx: S#Tx): Boolean = !valid() // !invalidVar.isFresh || (invalidVar.getOrElse(0) != 0)

    //    private[event] def isInvalid(slot: Int)(implicit tx: S#Tx): Boolean =
    //      !invalidVar.isFresh || ((invalidVar.getOrElse(0) & (1 << slot)) != 0)

    //    private[event] def validated(slot: Int)(implicit tx: S#Tx) {
    //      val mask = ~(1 << slot)
    //      if (invalidVar.isFresh) {
    //        invalidVar.transform(0)(_ & mask)
    //      } else {
    //        invalidVar() = mask
    //      }
    //    }

    //    private[event] def invalidate(slot: Int)(implicit tx: S#Tx) {
    //      if (invalidVar.isFresh) {
    //        invalidVar.transform(0)(_ | (1 << slot))
    //      } else {
    //        invalidVar() = 0xFFFFFFFF
    //      }
    //    }

    private[event] def validated()(implicit tx: S#Tx): Unit = valid.update()

    //    private[event] def invalidate()(implicit tx: S#Tx): Unit = {
    //      invalidVar() = 0xFFFFFFFF
    //    }
  }
}

/**
 * An abstract trait unifying invariant and mutating targets. This object is responsible
 * for keeping track of the dependents of an event source which is defined as the outer
 * object, sharing the same `id` as its targets. As a `Reactor`, it has a method to
 * `propagate` a fired event.
 */
sealed trait Targets[S <: stm.Sys[S]] extends Reactor[S] /* extends Writable with Disposable[ S#Tx ] */ {
  private[event] def children(implicit tx: S#Tx): Children[S]

  private[lucre] def isPartial: Boolean

  /**
   * Adds a dependant to this node target.
   *
   * @param slot the slot for this node to be pushing to the dependant
   * @param sel  the target selector to which an event at slot `slot` will be pushed
   *
   * @return  `true` if this was the first dependant registered with the given slot, `false` otherwise
   */
  private[event] def add(slot: Int, sel: /* MMM Expanded */ Selector[S])(implicit tx: S#Tx): Boolean

  /**
   * This method should be invoked when the targets are invalid for the given slot. It resets the children
   * for that slot to the single selector, and clears the invalid flag for the slot.
   *
   * @param slot the slot for this node to be pushing to the dependant
   * @param sel  the target selector to which an event at slot `slot` will be pushed
   */
  private[event] def resetAndValidate(slot: Int, sel: /* MMM Expanded */ Selector[S])(implicit tx: S#Tx): Unit

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  /**
   * Removes a dependant from this node target.
   *
   * @param slot the slot for this node which is currently pushing to the dependant
   * @param sel  the target selector which was registered with the slot
   *
   * @return  `true` if this was the last dependant unregistered with the given slot, `false` otherwise
   */
  private[event] def remove(slot: Int, sel: /* MMM Expanded */ Selector[S])(implicit tx: S#Tx): Boolean

  private[event] def observers(implicit tx: S#Tx): IIdxSeq[ObserverKey[S]]

  private[event] def isInvalid   (implicit tx: S#Tx): Boolean
  private[event] def validated ()(implicit tx: S#Tx): Unit
  // private[event] def invalidate()(implicit tx: S#Tx): Unit

  //  private[event] def isInvalid (slot: Int)(implicit tx: S#Tx): Boolean
  //  private[event] def validated (slot: Int)(implicit tx: S#Tx): Unit
  //  private[event] def invalidate(slot: Int)(implicit tx: S#Tx): Unit
}

/**
 * An `Event.Node` is most similar to EScala's `EventNode` class. It represents an observable
 * object and can also act as an observer itself. It adds the `Reactor` functionality in the
 * form of a proxy, forwarding to internally stored `Targets`. It also provides a final
 * implementation of the `Writable` and `Disposable` traits, asking sub classes to provide
 * methods `writeData` and `disposeData`. That way it is ensured that the sealed `Reactor` trait
 * is written first as the `Targets` stub, providing a means for partial deserialization during
 * the push phase of event propagation.
 *
 * This trait also implements `equals` and `hashCode` in terms of the `id` inherited from the
 * targets.
 */
trait Node[S <: stm.Sys[S]] extends /* Reactor[ S ] with */ VirtualNode[S] /* with Dispatcher[ S, A ] */ {
  override def toString = "Node" + id

  protected def targets: Targets[S]
  protected def writeData(out: DataOutput): Unit
  protected def disposeData()(implicit tx: S#Tx): Unit

  final protected def validated() (implicit tx: S#Tx)  { targets.validated() }
  final protected def isInvalid   (implicit tx: S#Tx): Boolean = targets.isInvalid
  // final protected def invalidate()(implicit tx: S#Tx): Unit = targets.invalidate()

  final private[event] def _targets: Targets[S] = targets

  final private[event] def children(implicit tx: S#Tx) = targets.children

  final def id: S#ID = targets.id

  private[event] def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] // NodeSelector[ S, Any ]

  final def write(out: DataOutput): Unit = {
    targets.write(out)
    writeData(out)
  }

  final def dispose()(implicit tx: S#Tx): Unit = {
    disposeData() // call this first, as it may release events
    targets.dispose()
  }
}

/**
 * The `Reactor` trait encompasses the possible targets (dependents) of an event. It defines
 * the `propagate` method which is used in the push-phase (first phase) of propagation. A `Reactor` is
 * either a persisted event `Node` or a registered `ObserverKey` which is resolved through the transaction
 * as pointing to a live view.
 */
sealed trait Reactor[S <: stm.Sys[S]] extends Mutable[S#ID, S#Tx] {
  private[event] def _targets: Targets[S]

  override def equals(that: Any): Boolean = that match {
    case value: Reactor[_] => id == value.id
    case _ => super.equals(that)
  }

  override def hashCode = id.hashCode()
}

object VirtualNode {
  private[event] def read[S <: Sys[S]](in: DataInput, fullSize: Int, access: S#Acc)(implicit tx: S#Tx): Raw[S] = {
    val off       = in.position
    val targets   = Targets.read(in, access)
    val dataSize  = fullSize - (in.position - off)
    val data      = new Array[Byte](dataSize)
    in.readFully(data)
    new Raw(targets, data, access)
  }

  private[event] final class Raw[S <: Sys[S]](private[event] val _targets: Targets[S], data: Array[Byte], access: S#Acc)
    extends VirtualNode[S] {

    def id: S#ID = _targets.id

    def write(out: DataOutput): Unit = {
      _targets.write(out)
      out.write(data)
    }

    private[event] def select(slot: Int /*, invariant: Boolean */) = Selector(slot, this /*, invariant */)

    private[event] def devirtualize[Repr](reader: Reader[S, Repr])(implicit tx: S#Tx): Repr with Node[S] = {
      val in = DataInput(data)
      reader.read(in, access, _targets)
    }

    def dispose()(implicit tx: S#Tx): Unit = _targets.dispose()

    override def toString = "VirtualNode.Raw" + _targets.id
  }
}

/** A virtual node is either in "raw", serialized state, or expanded to a full `Node`. */
sealed trait VirtualNode[S <: stm.Sys[S]] extends Reactor[S] {
  private[event] def select(slot: Int /*, invariant: Boolean */): VirtualNodeSelector[S]
}

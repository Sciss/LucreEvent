/*
 *  DurableImpl.scala
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

package de.sciss.lucre
package event
package impl

import stm.{DataStoreFactory, DataStore}
import concurrent.stm.{Ref, InTxn}

object DurableImpl {
  def apply(factory: DataStoreFactory[DataStore], mainName: String, eventName: String): Durable = {
    val mainStore   = factory.open(mainName)
    val eventStore  = factory.open(eventName)
    new DurableSystem(mainStore, eventStore)
  }

  private type D[S <: DurableLike[S]] = DurableLike[S]

  private sealed trait DurableSource[S <: D[S], @specialized(Int) A] extends event.Var[S, A] {
    protected def id: Int

    final def write(out: DataOutput) {
      out.writeInt(id)
    }

    final def dispose()(implicit tx: S#Tx) {
      tx.system.removeEvent(id)
    }

    final def getOrElse(default: => A)(implicit tx: S#Tx): A = get.getOrElse(default)

    final def transform(default: => A)(f: A => A)(implicit tx: S#Tx) {
      this() = f(getOrElse(default))
    }

    final def isFresh(implicit tx: S#Tx): Boolean = true
  }

  private final class DurableVarImpl[S <: D[S], A](protected val id: Int,
                                                   protected val ser: stm.Serializer[S#Tx, S#Acc, A])
    extends DurableSource[S, A] {

    def get(implicit tx: S#Tx): Option[A] = tx.system.tryReadEvent[A](id)(ser.read(_, ()))

    def update(v: A)(implicit tx: S#Tx) {
      tx.system.writeEvent(id)(ser.write(v, _))
    }

    override def toString = "event.Var(" + id + ")"
  }

  private final class DurableIntVar[S <: D[S]](protected val id: Int)
    extends DurableSource[S, Int] {

    def get(implicit tx: S#Tx): Option[Int] = {
      tx.system.tryReadEvent[Int](id)(_.readInt())
    }

    def update(v: Int)(implicit tx: S#Tx) {
      tx.system.writeEvent(id)(_.writeInt(v))
    }

    override def toString = "event.Var[Int](" + id + ")"
  }

  trait DurableMixin[S <: D[S], I <: Sys[I]] extends stm.impl.DurableImpl.Mixin[S, I] with DurableLike[S] {
    private val idCntVar = Ref(0)

    protected def eventStore: DataStore

    override def close() {
      super.close()
      eventStore.close()
    }

    private[event] def tryReadEvent[A](id: Int)(valueFun: DataInput => A)(implicit tx: S#Tx): Option[A] = {
      log("readE  <" + id + ">")
      eventStore.get(_.writeInt(id))(valueFun)
    }

    private[event] def writeEvent(id: Int)(valueFun: DataOutput => Unit)(implicit tx: S#Tx) {
      log("writE <" + id + ">")
      eventStore.put(_.writeInt(id))(valueFun)
    }

    private[event] def removeEvent(id: Int)(implicit tx: S#Tx) {
      log("remoE <" + id + ">")
      eventStore.remove(_.writeInt(id))
    }

    private[event] def newEventIDValue()(implicit tx: S#Tx): Int = {
      implicit val itx = tx.peer
      val id = idCntVar.get + 1
      log("newE  <" + id + ">")
      idCntVar.set(id)
      id
    }
  }

  trait DurableTxnMixin[S <: D[S]] extends DurableLike.Txn[S] {
    _: S#Tx =>

    final private[lucre] def reactionMap: ReactionMap[S] = system.reactionMap

    final private[event] def newEventVar[A](id: S#ID)
                                           (implicit serializer: stm.Serializer[S#Tx, S#Acc, A]): Var[S, A] = {
      new DurableVarImpl[S, A](system.newEventIDValue()(this), serializer)
    }

    final private[event] def newEventIntVar[A](id: S#ID): Var[S, Int] = {
      new DurableIntVar[S](system.newEventIDValue()(this))
    }

    final private[event] def readEventVar[A](id: S#ID, in: DataInput)
                                            (implicit serializer: stm.Serializer[S#Tx, S#Acc, A]): Var[S, A] = {
      val id = in.readInt()
      new DurableVarImpl[S, A](id, serializer)
    }

    final private[event] def readEventIntVar[A](id: S#ID, in: DataInput): Var[S, Int] = {
      val id = in.readInt()
      new DurableIntVar[S](id)
    }
  }

  private final class TxnImpl(val system: DurableSystem, val peer: InTxn)
    extends stm.impl.DurableImpl.TxnMixin[Durable] with DurableTxnMixin[Durable] {
    lazy val inMemory: InMemory#Tx = system.inMemory.wrap(peer)

    override def toString = "event.Durable#Tx@" + hashCode.toHexString
  }

  // OBSOLETE: (( Important: DurableMixin after stm.impl.DurableImpl.Mixin, so that
  // it can properly override `close` to include the second store ))
  private final class DurableSystem(protected val store: DataStore, protected val eventStore: DataStore)
    extends DurableMixin[Durable, InMemory] with Durable with ReactionMapImpl.Mixin[Durable] {

    private type S = Durable
    val inMemory: InMemory = InMemory()

    def wrap(peer: InTxn): S#Tx = new TxnImpl(this, peer)

    override def toString = "event.Durable@" + hashCode.toHexString
  }
}

/*
 *  InMemoryImpl.scala
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
package event
package impl

import de.sciss.lucre.stm.impl.{InMemoryImpl => STMImpl}
import de.sciss.serial.{DataInput, DataOutput}

import scala.concurrent.stm.{InTxn, Ref}

object InMemoryImpl {
  def apply(): InMemory = new System

  private def opNotSupported(name: String): Nothing = sys.error(s"Operation not supported: $name")

  private sealed trait BasicVar[S <: Sys[S], /* @specialized(Int) */ A]
    extends Var[S, A] {

    override def toString = s"event.Var<${hashCode().toHexString}>"

    final def write(out: DataOutput) = ()

    // final def isFresh(implicit tx: S#Tx): Boolean = true
  }

  private final class VarImpl[S <: Sys[S], A](peer: Ref[A])
    extends BasicVar[S, A] {

    def get(implicit tx: S#Tx): Option[A] = Option(peer.get(tx.peer))

    def getOrElse(default: => A)(implicit tx: S#Tx): A = {
      val v = peer.get(tx.peer)
      if (v == null) default else v
    }

    def transform(default: => A)(f: A => A)(implicit tx: S#Tx): Unit =
      peer.transform(v => f(if (v == null) default else v))(tx.peer)

    def update(v: A)(implicit tx: S#Tx): Unit = peer.set(v)(tx.peer)
    def dispose   ()(implicit tx: S#Tx): Unit = peer.set(null.asInstanceOf[A])(tx.peer)
  }

  //  private final class IntVarImpl[S <: Sys[S]](peer: Ref[Long])
  //    extends BasicVar[S, Int] {
  //    def get(implicit tx: S#Tx): Option[Int] = {
  //      val v = peer.get(tx.peer)
  //      if (v < 0) None else Some(v.toInt)
  //    }
  //
  //    def getOrElse(default: => Int)(implicit tx: S#Tx): Int = {
  //      val v = peer.get(tx.peer)
  //      if (v < 0) default else v.toInt
  //    }
  //
  //    def transform(default: => Int)(f: Int => Int)(implicit tx: S#Tx): Unit = {
  //      peer.transform(v => f(if (v < 0) default else v.toInt))(tx.peer)
  //    }
  //
  //    def update(v: Int)(implicit tx: S#Tx): Unit = {
  //      peer.set(v.toLong & 0xFFFFFFFFL)(tx.peer)
  //    }
  //
  //    def dispose()(implicit tx: S#Tx): Unit = {
  //      peer.set(-1L)(tx.peer)
  //    }
  //  }

  trait TxnMixin[S <: Sys[S]] extends Txn[S] {
    final private[lucre] def reactionMap: ReactionMap[S] = system.reactionMap

    final private[event] def newEventVar[A](id: S#ID)
                                           (implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Var[S, A] = {
      new VarImpl(Ref.make[A])
    }

    //    final private[event] def newEventIntVar[A](id: S#ID): Var[S, Int] = {
    //      new IntVarImpl(Ref(-1L))
    //    }

    final private[event] def readEventVar[A](id: S#ID, in: DataInput)
                                            (implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Var[S, A] = {
      opNotSupported("readEventVar")
    }

    //    final private[event] def readEventIntVar[A](id: S#ID, in: DataInput): Var[S, Int] = {
    //      opNotSupported("readEventIntVar")
    //    }

    final private[event] def readEventValidity(id: S#ID, in: DataInput): Validity[S#Tx] = DummyValidity
    final private[event] def newEventValidity (id: S#ID)               : Validity[S#Tx] = DummyValidity
  }

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends STMImpl.TxnMixin[InMemory] with TxnMixin[InMemory] {
    override def toString = s"event.InMemory#Tx@${hashCode.toHexString}"

    def inMemory: InMemory#Tx = this
  }

  private final class System extends STMImpl.Mixin[InMemory] with InMemory with ReactionMapImpl.Mixin[InMemory] {
    private type S = InMemory

    def wrap(peer: InTxn): S#Tx = new TxnImpl(this, peer)

    def inMemory: S = this
    def inMemoryTx(tx: Tx): Tx = tx

    override def toString = s"event.InMemory@${hashCode.toHexString}"
  }

}

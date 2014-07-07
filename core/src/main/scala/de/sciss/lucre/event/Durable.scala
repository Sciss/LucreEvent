/*
 *  Durable.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
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

import stm.{DataStoreFactory, DataStore}
import serial.{DataInput, DataOutput}
import impl.{DurableImpl => Impl}
import language.implicitConversions

object Durable {
   private type S = Durable

  def apply(factory: DataStoreFactory[DataStore], mainName: String = "data", eventName: String = "event"): S =
    Impl(factory, mainName, eventName)

  def apply(mainStore: DataStore, eventStore: DataStore): S = Impl(mainStore, eventStore)

  // a rare moment of love for Scala today ... this view is automatically found. at least something...
  // implicit def inMemory(tx: Durable#Tx): InMemory#Tx = tx.inMemory
}

object DurableLike {
  trait Txn[S <: DurableLike[S]] extends stm.DurableLike.Txn[S] with event.Txn[S] {
    // def inMemory: InMemory#Tx
  }
}

trait DurableLike[S <: DurableLike[S]] extends stm.DurableLike[S] with Sys[S] {
  type Tx <: DurableLike.Txn[S]

  private[event] def tryReadEvent[A](id: Int)(valueFun: DataInput => A)(implicit tx: S#Tx): Option[A]
  private[event] def writeEvent     (id: Int)(valueFun: DataOutput => Unit)(implicit tx: S#Tx): Unit
  private[event] def removeEvent    (id: Int)(implicit tx: S#Tx): Unit
  private[event] def newEventIDValue()(implicit tx: S#Tx): Int
}

trait Durable extends DurableLike[Durable] {
  // private type S = Durable
  type         I = InMemory
  type        Tx = DurableLike.Txn[Durable]
}
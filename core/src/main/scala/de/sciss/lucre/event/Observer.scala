/*
 *  Observer.scala
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

package de.sciss.lucre
package event

import stm.Disposable

object Observer {
  def apply[S <: Sys[S], A, Repr](event: Event[S, A, Repr], reader: Reader[S, Repr], fun: S#Tx => A => Unit)
                                 (implicit tx: S#Tx): Disposable[S#Tx] = {
    val key = tx.reactionMap.addEventReaction[A, Repr](reader, fun)
    val res = new Impl[S, A, Repr](event, key)
    event ---> key
    res
  }

  private final class Impl[S <: Sys[S], A, Repr](event: Event[S, A, Repr], key: ObserverKey[S])
    extends Disposable[S#Tx] {

    override def toString = "Observer<" + key.id + ">"

    def dispose()(implicit tx: S#Tx): Unit = {
      event -/-> key
      tx.reactionMap.removeEventReaction(key)
    }
  }

  /** This method is cheap. */
  def dummy[S <: stm.Sys[S]]: Disposable[S#Tx] = dummyVal.asInstanceOf[Disposable[S#Tx]]

  private val dummyVal = new Dummy[stm.InMemory]

  private final class Dummy[S <: stm.Sys[S]] extends Disposable[S#Tx] {
    override def toString = "Observer.Dummy"

    def dispose()(implicit tx: S#Tx) = ()
  }
}
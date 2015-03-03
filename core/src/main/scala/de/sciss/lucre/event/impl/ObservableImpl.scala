/*
 *  ObservableImpl.scala
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

package de.sciss.lucre.event
package impl

import de.sciss.lucre.stm.{Disposable, Sys}

import scala.concurrent.stm.Ref

object DummyObservableImpl extends Disposable[Any] {
  def dispose()(implicit tx: Any): Unit = ()
}
trait DummyObservableImpl[S <: Sys[S]] extends Observable[S#Tx, Nothing] {
  def react(fun: S#Tx => Nothing => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = DummyObservableImpl
}

trait ObservableImpl[S <: Sys[S], U] extends Observable[S#Tx, U] {
  private final class Observation(val fun: S#Tx => U => Unit) extends Disposable[S#Tx] {
    def dispose()(implicit tx: S#Tx): Unit = removeObservation(this)
  }

  private val obsRef = Ref(Vector.empty[Observation])

  final protected def fire(update: U)(implicit tx: S#Tx): Unit = {
    val obs = obsRef.get(tx.peer)
    obs.foreach(_.fun(tx)(update))
  }

  private def removeObservation(obs: Observation)(implicit tx: S#Tx): Unit =
    obsRef.transform(_.filterNot(_ == obs))(tx.peer)

  final def react(fun: S#Tx => U => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val obs = new Observation(fun)
    obsRef.transform(_ :+ obs)(tx.peer)
    obs
  }
}
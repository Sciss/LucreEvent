/*
 *  Bang.scala
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
package event

import serial.DataInput

object Bang {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Bang[S] = new Impl[S](Targets[S])

  private final class Impl[S <: Sys[S]](protected val targets: Targets[S])
    extends Bang[S] with impl.StandaloneLike[S, Unit, Bang[S]] with impl.Singleton[S] with impl.Root[S, Unit]
    with impl.Generator[S, Unit, Bang[S]] {

    protected def reader = Bang.serializer[S]

    override def toString = "Bang"

    def apply()(implicit tx: S#Tx): Unit = fire(())
    def apply(unit: Unit)(implicit tx: S#Tx): Unit = fire(())
  }

  implicit def serializer[S <: Sys[S]]: NodeSerializer[S, Bang[S]] = new NodeSerializer[S, Bang[S]] {
    // note: there was a strange runtime error when using an anonymous class instead. It seems that
    // scala somehow missed to execute the body, leaving targets unassigned. Perhaps a bug
    // of scalac getting confused with the apply method?
    def read(in: DataInput, access: S#Acc, _targets: Targets[S])(implicit tx: S#Tx): Bang[S] =
      new Impl[S](_targets)
  }
}

/** A simple event implementation for an imperative (trigger) event that fires "bangs" or impulses, using the
  * `Unit` type as event type parameter. The `apply` method of the companion object builds a `Bang` which also
  * implements the `Observable` trait, so that the bang can be connected to a live view (e.g. a GUI).
  */
sealed trait Bang[S <: Sys[S]] extends Trigger[S, Unit, Bang[S]] with Node[S] {
  /** A parameterless convenience version of the `Trigger`'s `apply` method. */
  def apply()(implicit tx: S#Tx): Unit // { apply( () )}
}

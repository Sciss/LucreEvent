/*
 *  MappingGenerator.scala
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

package de.sciss.lucre
package event
package impl

/** A trait which combined external input events with self generated events. */
trait MappingGenerator[S <: Sys[S], A, B, +Repr]
  extends Generator  [S, A, Repr]
  with StandaloneLike[S, A, Repr] with Publisher[S, A] {
  _: Repr =>

  protected def inputEvent: EventLike[S, B]

  def changed: Event[S, A, Repr] = this

  /** Folds a new input event, by combining it with an optional previous output event. */
  protected def foldUpdate(generated: Option[A], input: B)(implicit tx: S#Tx): Option[A]

  final private[lucre] def connect   ()(implicit tx: S#Tx): Unit = inputEvent ---> this
  final private[lucre] def disconnect()(implicit tx: S#Tx): Unit = inputEvent -/-> this

  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = {
    val gen = if (pull.isOrigin(this)) Some(pull.resolve[A]) else None
    if (pull.contains(inputEvent)) pull(inputEvent) match {
      case Some(e)  => foldUpdate(gen, e)
      case _        => gen
    } else gen
  }
}
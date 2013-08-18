/*
 *  MappingGenerator.scala
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

/** A trait which combined external input events with self generated events. */
trait MappingGenerator[S <: Sys[S], A, B, +Repr]
  extends Generator[S, A, Repr]
  with StandaloneLike[S, A, Repr] {
  _: Repr =>

  protected def inputEvent: EventLike[S, B]

  def changed: Event[S, A, Repr] = this

  /** Folds a new input event, by combining it with an optional previous output event. */
  protected def foldUpdate(generated: Option[A], input: B)(implicit tx: S#Tx): Option[A]

  final private[lucre] def connect   ()(implicit tx: S#Tx): Unit = inputEvent ---> this
  final private[lucre] def disconnect()(implicit tx: S#Tx): Unit = inputEvent -/-> this

  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = {
    val gen = if (pull.isOrigin(this)) pull.resolve[A] else None
    if (pull.contains(inputEvent)) pull(inputEvent) match {
      case Some(e)  => foldUpdate(gen, e)
      case _        => gen
    } else gen
  }
}
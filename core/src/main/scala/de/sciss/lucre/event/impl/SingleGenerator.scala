/*
 *  SingleGenerator.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
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

/** Combines `Generator`, `Root` and `StandaloneLike` into a convenient trait that instantly
  * sets up a self contained rooted event, dispatched via `fire`. It also adds method
  * `changed` which is useful for distinguishing node and event on the outside facade.
  *
  * @tparam S     the system used
  * @tparam A     the update type
  * @tparam Repr  the representation type. This type mixin in this trait must conform to this (self-)type.
  */
trait SingleGenerator[S <: Sys[S], A, +Repr]
  extends Generator[S, A, Repr] with Root[S, A] with StandaloneLike[S, A, Repr] with Publisher[S, A] {
  _: Repr =>

  def changed: Event[S, A, Repr] = this
}
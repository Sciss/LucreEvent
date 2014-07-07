/*
 *  SingleGenerator.scala
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
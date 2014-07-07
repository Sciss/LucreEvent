/*
 *  Reducer.scala
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

/** A trait which reduces multiple input events into one output event.
  *
  * @tparam S     the system
  * @tparam A     the reduced (output) event type
  * @tparam B     the input event type
  * @tparam Repr  the self-type (implementing instance must conform to this)
  */
trait Reducer[S <: Sys[S], A, B, Repr] extends Node[S] with Publisher[S, A] {
  self: Repr =>

  /** A list of all input events. */
  protected def events: Traversable[Event[S, B, Repr]]

  protected def changedSlot: Int

  protected def reader: Reader[S, Repr]

  /** Folds a new input event, by combining it with an optional previous output event. */
  protected def foldUpdate(sum: Option[A], inc: B)(implicit tx: S#Tx): Option[A]

  object changed extends impl.EventImpl[S, A, Repr] with InvariantEvent[S, A, Repr] {
    def node: Repr with Node[S] = self

    override def toString = s"$node.changed"
    def slot: Int = changedSlot // events.size // throw new UnsupportedOperationException

    def connect   ()(implicit tx: S#Tx): Unit = events.foreach(_ ---> this)
    def disconnect()(implicit tx: S#Tx): Unit = events.foreach(_ -/-> this)

    protected def reader: Reader[S, Repr] = self.reader

    private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] =
      events.foldLeft(Option.empty[A]) {
        case (res, e) =>
          if (pull.contains(e)) {
            pull(e) match {
              case Some(upd) => foldUpdate(res, upd)
              case _ => res
            }
          } else res
      }
  }

  final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = {
    if (slot == changed.slot) changed else
    events.find(_.slot == slot) getOrElse sys.error(s"Invalid slot $slot")
  }
}
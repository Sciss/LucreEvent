/*
 *  Root.scala
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

/**
 * A rooted event does not have sources. This trait provides a simple
 * implementation of `pull` which merely checks if this event has fired or not.
 */
trait Root[S <: stm.Sys[S], +A] {
  final private[lucre] def connect   ()(implicit tx: S#Tx) = ()
  final private[lucre] def disconnect()(implicit tx: S#Tx) = ()

  final private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A] = Some(pull.resolve[A])
}

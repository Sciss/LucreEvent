/*
 *  Generator.scala
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

trait Generator[S <: Sys[S], A, +Repr] extends Event[S, A, Repr] {
  final protected def fire(update: A)(implicit tx: S#Tx): Unit = {
    log(this.toString + " fire " + update)
    Push(this /* select() */ , update)
  }
}
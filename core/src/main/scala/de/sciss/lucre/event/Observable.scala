/*
 *  Observable.scala
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

import de.sciss.lucre.stm.Disposable

trait Observable[Tx, +A] {
  /** Registers a live observer with this observable. The method is called with the
    * observing function which receives the observable's update message of type `A`, and the
    * method generates an opaque `Disposable` instance, which may be used to
    * remove the observer eventually (through the `dispose` method).
    */
  def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx]
}
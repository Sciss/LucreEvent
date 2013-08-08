/*
 *  Observer.scala
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

import stm.Disposable

object Observer {
  def apply[S <: Sys[S], A, Repr](event: Event[S, A, Repr], reader: Reader[S, Repr], fun: S#Tx => A => Unit)
                                 (implicit tx: S#Tx): Disposable[S#Tx] = {
    val key = tx.reactionMap.addEventReaction[A, Repr](reader, fun)
    val res = new Impl[S, A, Repr](event, key)
    event ---> key
    res
  }

  private final class Impl[S <: Sys[S], A, Repr](event: Event[S, A, Repr], key: ObserverKey[S])
    extends Disposable[S#Tx] {
    override def toString = "Observer<" + key.id + ">"

    // def add[R1 >: Repr /* <: Node[ S ] */ ](event: EventLike[S, A, R1])(implicit tx: S#Tx): Unit = {
    //  event ---> key
    // }

    // def remove[R1 >: Repr /* <: Node[ S ] */ ](event: EventLike[S, A, R1])(implicit tx: S#Tx): Unit = {
    //   event -/-> key
    // }

    def dispose()(implicit tx: S#Tx): Unit = {
      event -/-> key
      tx.reactionMap.removeEventReaction(key)
    }
  }

  /**
   * This method is cheap.
   */
  def dummy[S <: stm.Sys[S]]: Disposable[S#Tx] = dummyVal.asInstanceOf[Disposable[S#Tx]]

  private val dummyVal = new Dummy[stm.InMemory]

  private final class Dummy[S <: stm.Sys[S]] extends Disposable[S#Tx] {
    override def toString = "Observer.Dummy"

    def dispose()(implicit tx: S#Tx) = ()
  }
}

///**
// * `Observer` instances are returned by the `observe` method of classes implementing
// * `Observable`. The observe can be registered and unregistered with events.
// */
//sealed trait Observer[ S <: stm.Sys[ S ], -A, +Repr ] extends Disposable[ S#Tx ] {
//   def add[    R1 >: Repr /* <: Node[ S ] */]( event: EventLike[ S, A, R1 ])( implicit tx: S#Tx ) : Unit
//   def remove[ R1 >: Repr /* <: Node[ S ] */]( event: EventLike[ S, A, R1 ])( implicit tx: S#Tx ) : Unit
//}
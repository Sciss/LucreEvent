/*
 *  EventImpl.scala
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

package de.sciss.lucre
package event
package impl

import de.sciss.lucre.stm.Disposable

trait EventImpl[S <: Sys[S], +A, +Repr /* <: Node[ S ] */ ]
  extends Event[S, A, Repr] /* with InvariantSelector[ S ] */ {

  // final /* private[lucre] */ def isSource(pull: Pull[S]): Boolean = pull.hasVisited(this /* select() */)

  protected def reader: Reader[S, Repr]

  //   final /* private[lucre] */ def --->( r: ExpandedSelector[ S ])( implicit tx: S#Tx ): Unit = {
  //      if( reactor._targets.add( slot, r )) connect()
  //   }
  //
  //   final /* private[lucre] */ def -/->( r: ExpandedSelector[ S ])( implicit tx: S#Tx ): Unit = {
  //      if( reactor._targets.remove( slot, r )) disconnect()
  //   }

  final def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = Observer[S, A, Repr](this, reader, fun)
    // res.add(this)
    res
  }
}

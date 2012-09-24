/*
 *  Root.scala
 *  (LucreSTM)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

/**
 * A rooted event does not have sources. This trait provides a simple
 * implementation of `pull` which merely checks if this event has fired or not.
 */
trait Root[ S <: stm.Sys[ S ], +A ] /* extends Node[ S, A, Repr ] */ {
   final /* private[lucre] */ def connect()(    implicit tx: S#Tx ) {}
//   final private[lucre] def reconnect()(  implicit tx: S#Tx ) {}
   final /* private[lucre] */ def disconnect()( implicit tx: S#Tx ) {}

   final /* override */ /* private[lucre] */ def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ A ] = pull.resolve[ A ]
}

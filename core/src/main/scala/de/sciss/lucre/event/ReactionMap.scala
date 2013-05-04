/*
 *  ReactionMap.scala
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

import impl.{ReactionMapImpl => Impl}

object ReactionMap {
  def apply[S <: stm.Sys[S]](): ReactionMap[S] = Impl[S]
}

trait ReactionMap[S <: stm.Sys[S]] {
  def addEventReaction[A, Repr](reader: event.Reader[S, Repr], fun: S#Tx => A => Unit)
                               (implicit tx: S#Tx): ObserverKey[S]

  def removeEventReaction(key: ObserverKey[S])(implicit tx: S#Tx): Unit

  def processEvent(leaf: ObserverKey[S], parent: VirtualNodeSelector[S], push: Push[S])(implicit tx: S#Tx): Unit
}
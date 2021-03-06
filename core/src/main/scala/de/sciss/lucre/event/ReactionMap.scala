/*
 *  ReactionMap.scala
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

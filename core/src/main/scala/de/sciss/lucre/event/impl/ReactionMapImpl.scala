/*
 *  ReactionMapImpl.scala
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
package impl

import concurrent.stm.{Ref, TMap}

object ReactionMapImpl {
  private val noOpEval = () => ()
  private type AnyObsFun[S <: stm.Sys[S]] = S#Tx => AnyRef => Unit

  def apply[S <: stm.Sys[S]]: ReactionMap[S] = new Impl[S]

  private final case class EventObservation[S <: stm.Sys[S], -A](reader: event.Reader[S, Any],
                                                                 fun: S#Tx => A => Unit) {

    def reaction(parent: VirtualNodeSelector[S], pull: Pull[S])(implicit tx: S#Tx): Reaction = {
      val nParent = parent.devirtualize[A, Any](reader)
      () => {
        pull(nParent) match {
          case Some(result) =>
            () => fun(tx)(result)
          case None => noOpEval
        }
      }
    }
  }

   private final class Impl[ S <: stm.Sys[ S ]] extends Mixin[ S ] {
      override def toString = "ReactionMap@" + hashCode.toHexString
   }

  trait Mixin[S <: stm.Sys[S]] extends ReactionMap[S] {
    private val cnt       = Ref(0)
    private val eventMap  = TMap.empty[Int, EventObservation[S, Nothing]]

    // self-reference useful when Mixin is added to an event.Sys
    def reactionMap: ReactionMap[S] = this

    final def processEvent(leaf: ObserverKey[S], parent: VirtualNodeSelector[S], push: Push[S])
                          (implicit tx: S#Tx): Unit = {
      val itx = tx.peer
      eventMap.get(leaf.id)(itx).foreach { obs =>
        val react = obs.reaction(parent, push)
        push.addReaction(react)
      }
    }

    final def addEventReaction[A, Repr](reader: event.Reader[S, Repr], fun: S#Tx => A => Unit)
                                       (implicit tx: S#Tx): ObserverKey[S] = {
      implicit val itx = tx.peer
      val key = cnt.get
      cnt.set(key + 1)
      eventMap.+=((key, new EventObservation[S, A](reader, fun)))(tx.peer)
      new ObserverKey[S](key)
    }

    def removeEventReaction(key: ObserverKey[S])(implicit tx: S#Tx): Unit =
      eventMap.-=(key.id)(tx.peer)
  }
}

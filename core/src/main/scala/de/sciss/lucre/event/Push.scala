/*
 *  Push.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
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

import collection.immutable.{IndexedSeq => Vec}
import annotation.elidable
import elidable.CONFIG

object Push {
  private[event] def apply[S <: Sys[S], A](origin: Event[S, A, Any], update: A)(implicit tx: S#Tx): Unit = {
    val push = new Impl(origin, update)
    log("push begin")
    //      resetIndent()
    //      val inlet   = source.slot
    //      source.reactor.children.foreach { tup =>
    //         val inlet2 = tup._1
    //         if( inlet2 == inlet ) {
    //            val sel = tup._2
    //            sel.pushUpdate( source, push )
    //         }
    //      }
    push.visitChildren(origin)
    log("pull begin")
    push.pull()
    log("pull end")
  }

  private val NoReactions = Vec.empty[Reaction]
  //   private val emptySet = Set.empty[ Nothing ]
  //   private val emptyMap = Map.empty[ Nothing, Nothing ]
  type Parents[S <: stm.Sys[S]] = Set[VirtualNodeSelector[S]]

  private def NoParents [S <: stm.Sys[S]]: Parents[S] = Set.empty[VirtualNodeSelector[S]]
  // private def NoMutating[S <: stm.Sys[S]]: Set[MutatingSelector[S]] = Set.empty[MutatingSelector[S]]

  private type Visited[S <: stm.Sys[S]] = Map[VirtualNodeSelector[S], Parents[S]]

  private final class Impl[S <: Sys[S]](origin: Event[S, Any, Any], val update: Any)(implicit tx: S#Tx)
    extends Push[S] {
    private var pushMap   = Map((origin: Any, NoParents[S]))
    private var pullMap   = Map.empty[EventLike[S, Any], Option[Any]]
    private var reactions = NoReactions
    // private var mutating  = NoMutating[S]

    private var indent    = ""

    //      @elidable(CONFIG) private def resetIndent(): Unit = indent = ""
    @elidable(CONFIG) private def incIndent(): Unit = indent += "  "
    @elidable(CONFIG) private def decIndent(): Unit = indent = indent.substring(2)

    private def addVisited(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]): Boolean = {
      val parents = pushMap.getOrElse(sel, NoParents)
      log(s"${indent}visit $sel  (new ? ${parents.isEmpty})")
      pushMap += ((sel, parents + parent))
      parents.isEmpty
    }

    def visitChildren(sel: VirtualNodeSelector[S]): Unit = {
      val inlet = sel.slot
      incIndent()
      try {
        val ch = sel.node._targets.children
        ch.foreach { tup =>
          val inlet2 = tup._1
          if (inlet2 == inlet) {
            val selChild = tup._2
            selChild.pushUpdate(sel, this)
          }
        }
      } finally {
        decIndent()
      }
    }

    def visit(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]): Unit =
      if (addVisited(sel, parent)) visitChildren(sel)

    //      def visit( sel: MutatingSelector[ S ], parent: VirtualNodeSelector[ S ]): Unit = {
    //         if( addVisited( sel, parent )) {
    //            mutating += sel
    //            visitChildren( sel )
    //         }
    //      }

    def contains(source: EventLike[S, Any]): Boolean = pushMap.contains(source)

    def isOrigin(that: EventLike[S, Any]) = that == origin

    def parents(sel: VirtualNodeSelector[S]): Parents[S] = pushMap.getOrElse(sel, NoParents)

    def addLeaf(leaf: ObserverKey[S], parent: VirtualNodeSelector[S]): Unit = {
      log(s"${indent}addLeaf $leaf, parent = $parent")
      tx.reactionMap.processEvent(leaf, parent, this)
    }

    def addReaction(r: Reaction): Unit = reactions :+= r

    def pull(): Unit = {
      log(s"numReactions = ${reactions.size}")
      val firstPass = reactions.map(_.apply())
      /* val secondPass = */ firstPass.foreach(_.apply())

      //      if (mutating.nonEmpty) {
      //        log("numInvalid = " + mutating.size)
      //        mutating.foreach { sel =>
      //          println("INVALIDATED: " + mutating.mkString(", "))
      //          sel.node._targets.invalidate(sel.slot)
      //        }
      //      }
    }

    //    def markInvalid(evt: MutatingSelector[S]): Unit = {
    //      log("markInvalid " + evt)
    //      mutating += evt
    //    }
    //
    //    def clearInvalid(evt: MutatingSelector[S]): Unit = {
    //      log("clearInvalid " + evt)
    //      mutating -= evt
    //    }

    def resolve[A]: A = {
      log(s"${indent}resolve")
      update.asInstanceOf[A]
    }

    // caches pulled values
    def apply[A](source: EventLike[S, A]): Option[A] = {
      incIndent()
      try {
        pullMap.get(source) match {
          case Some(res: Option[_]) =>
            log(s"${indent}pull $source  (new ? false)")
            res.asInstanceOf[Option[A]]
          case _ =>
            log(s"${indent}pull $source  (new ? true)")
            val res = source.pullUpdate(this)
            pullMap += ((source, res))
            res
        }
      } finally {
        decIndent()
      }
    }
  }
}

sealed trait Pull[S <: stm.Sys[S]] {
  /** Assuming that the caller is origin of the event, resolves the update of the given type. */
  def resolve[A]: A

  // def update: Any

  /** Retrieves the immediate parents from the push phase. */
  def parents(sel: VirtualNodeSelector[S]): Push.Parents[S]

  /** Pulls the update from the given source. */
  def apply[A](source: EventLike[S, A]): Option[A]

  /** Whether the selector has been visited during the push phase. */
  /* private[event] */ def contains(source: EventLike[S, Any]): Boolean
  def isOrigin(source: EventLike[S, Any]): Boolean

  // private[event] def clearInvalid(evt: MutatingSelector[S])
}

private[event] sealed trait Push[S <: stm.Sys[S]] extends Pull[S] {
  private[event] def visit(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]): Unit

  //   def visit( sel: MutatingSelector[ S ],  parent: VirtualNodeSelector[ S ]) : Unit
  //   def mutatingVisit( sel: VirtualNodeSelector[ S ], parent: VirtualNodeSelector[ S ]) : Unit
  //   def addMutation( sel: VirtualNodeSelector[ S ]) : Unit

  private[event] def addLeaf(leaf: ObserverKey[S], parent: VirtualNodeSelector[S]): Unit

  private[event] def addReaction(r: Reaction): Unit

  // private[event] def markInvalid(evt: MutatingSelector[S])
}
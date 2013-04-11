/*
 *  Push.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import annotation.elidable
import elidable.CONFIG

object Push {
  private[event] def apply[S <: Sys[S], A](source: Event[S, A, Any], update: A)(implicit tx: S#Tx) {
    val push = new Impl(source, update)
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
    push.visitChildren(source)
    log("pull begin")
    push.pull()
    log("pull end")
  }

  private val NoReactions = IIdxSeq.empty[Reaction]
  //   private val emptySet = Set.empty[ Nothing ]
  //   private val emptyMap = Map.empty[ Nothing, Nothing ]
  type Parents[S <: stm.Sys[S]] = Set[VirtualNodeSelector[S]]

  private def NoParents [S <: stm.Sys[S]]: Parents[S] = Set.empty[VirtualNodeSelector[S]]
  private def NoMutating[S <: stm.Sys[S]]: Set[MutatingSelector[S]] = Set.empty[MutatingSelector[S]]

  private type Visited[S <: stm.Sys[S]] = Map[VirtualNodeSelector[S], Parents[S]]

  private final class Impl[S <: Sys[S]](source: VirtualNodeSelector[S], val update: Any)(implicit tx: S#Tx)
    extends Push[S] {
    private var visited = Map((source, NoParents[S]))
    // EmptyVisited[ S ]
    private var reactions = NoReactions
    private var mutating = NoMutating[S]

    private var indent = ""

    //      @elidable(CONFIG) private def resetIndent() { indent = "" }
    @elidable(CONFIG) private def incIndent() {
      indent += "  "
    }

    @elidable(CONFIG) private def decIndent() {
      indent = indent.substring(2)
    }

    private def addVisited(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]): Boolean = {
      val parents = visited.getOrElse(sel, NoParents)
      log(indent + "visit " + sel + " (new ? " + parents.isEmpty + ")")
      visited += ((sel, parents + parent))
      parents.isEmpty
    }

    def visitChildren(sel: VirtualNodeSelector[S]) {
      val inlet = sel.slot
      incIndent()
      try {
        val ch = sel.node._targets.children
        ch.foreach {
          tup =>
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

    def visit(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]) {
      if (addVisited(sel, parent)) visitChildren(sel)
    }

    //      def visit( sel: MutatingSelector[ S ], parent: VirtualNodeSelector[ S ]) {
    //         if( addVisited( sel, parent )) {
    //            mutating += sel
    //            visitChildren( sel )
    //         }
    //      }

    def hasVisited(sel: VirtualNodeSelector[S]): Boolean = visited.contains(sel)

    def parents(sel: VirtualNodeSelector[S]): Parents[S] = visited.getOrElse(sel, NoParents)

    def addLeaf(leaf: ObserverKey[S], parent: VirtualNodeSelector[S]) {
      log(indent + "addLeaf " + leaf + ", parent = " + parent)
      tx.reactionMap.processEvent(leaf, parent, this)
    }

    def addReaction(r: Reaction) {
      reactions :+= r
    }

    def pull() {
      log("numReactions = " + reactions.size)
      val firstPass = reactions.map(_.apply())
      /* val secondPass = */ firstPass.foreach(_.apply())

      if (mutating.nonEmpty) {
        log("numInvalid = " + mutating.size)
        mutating.foreach { sel =>
          println("INVALIDATED: " + mutating.mkString(", "))
          sel.node._targets.invalidate(sel.slot)
        }
      }
    }

    def markInvalid(evt: MutatingSelector[S]) {
      log("markInvalid " + evt)
      mutating += evt
    }

    def clearInvalid(evt: MutatingSelector[S]) {
      log("clearInvalid " + evt)
      mutating -= evt
    }

    def resolve[A]: Option[A] = Some(update.asInstanceOf[A])
  }
}

sealed trait Pull[S <: stm.Sys[S]] {
  def resolve[A]: Option[A]
  def update: Any
  def hasVisited  (sel: VirtualNodeSelector[S]): Boolean
  def parents     (sel: VirtualNodeSelector[S]): Push.Parents[S]
  def clearInvalid(evt: MutatingSelector[S])
}

sealed trait Push[S <: stm.Sys[S]] extends Pull[S] {
  def visit(sel: VirtualNodeSelector[S], parent: VirtualNodeSelector[S]): Unit

  //   def visit( sel: MutatingSelector[ S ],  parent: VirtualNodeSelector[ S ]) : Unit
  //   def mutatingVisit( sel: VirtualNodeSelector[ S ], parent: VirtualNodeSelector[ S ]) : Unit
  //   def addMutation( sel: VirtualNodeSelector[ S ]) : Unit
  def addLeaf(leaf: ObserverKey[S], parent: VirtualNodeSelector[S]): Unit

  def addReaction(r: Reaction): Unit

  def markInvalid(evt: MutatingSelector[S])
}
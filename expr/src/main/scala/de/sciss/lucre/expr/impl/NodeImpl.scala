/*
 *  NodeImpl.scala
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
package expr
package impl

import event.{impl => eimpl, Event, InvariantSelector}
import de.sciss.model.Change
import expr.{String => _String}

trait NodeImpl[S <: event.Sys[S], A]
  extends Expr.Node[S, A]
  with eimpl.StandaloneLike[S, Change[A], Expr[S, A]] with InvariantSelector[S] {

  final def changed: Event[S, Change[A], Expr[S, A]] = this

  final def disposeData()(implicit tx: S#Tx) = ()

  override def toString = "Expr" + id
}
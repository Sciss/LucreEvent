package de.sciss.lucre
package expr
package impl
import event.{impl => eimpl, Event, InvariantSelector}
import de.sciss.model.Change

trait NodeImpl[S <: event.Sys[S], A] extends Expr.Node[S, A]
with eimpl.StandaloneLike[S, Change[A], Expr[S, A]] with InvariantSelector[S] {

  final def changed: Event[S, Change[A], Expr[S, A]] = this

  final def disposeData()(implicit tx: S#Tx) = ()

  override def toString = "Expr" + id
}
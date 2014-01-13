package de.sciss.lucre
package expr
package impl
import event.{impl => eimpl, Sys}

trait ConstImpl[S <: Sys[S], A] extends Expr.Const[S, A] with eimpl.Constant

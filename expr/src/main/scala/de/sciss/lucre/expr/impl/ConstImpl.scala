package de.sciss.lucre
package expr
package impl
import event.{impl => eimpl}

trait ConstImpl[ S <: stm.Sys[ S ], A ] extends Expr.Const[ S, A ] with eimpl.Constant

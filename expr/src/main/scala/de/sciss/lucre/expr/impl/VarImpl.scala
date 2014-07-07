/*
 *  VarImpl.scala
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

package de.sciss
package lucre
package expr
package impl

import event.{Pull, Event, InvariantSelector, impl => evti}
import serial.DataOutput
import de.sciss.model.Change
import expr.{String => _String}

trait VarImpl[S <: event.Sys[S], A]
  extends Expr.Var[S, A] with evti.StandaloneLike[S, Change[A], Expr[S, A]]
  with evti.Generator[S, Change[A], Expr[S, A]] with InvariantSelector[S] {
  expr =>

  private type Ex = Expr[S, A]

  final def changed: Event[S, Change[A], Expr[S, A]] = this // changedImp

  // ---- these need to be implemented by subtypes ----
  protected def ref: S#Var[Ex]

  protected def reader: event.Reader[S, Expr[S, A]]

  final protected def writeData(out: DataOutput): Unit = {
    out.writeByte(0)
    ref.write(out)
  }

  final protected def disposeData()(implicit tx: S#Tx): Unit =ref.dispose()

  final private[lucre] def connect   ()(implicit tx: S#Tx): Unit = ref().changed ---> this
  final private[lucre] def disconnect()(implicit tx: S#Tx): Unit = ref().changed -/-> this

  final def apply()(implicit tx: S#Tx): Ex = ref()

  final def update(expr: Ex)(implicit tx: S#Tx): Unit = {
    val before = ref()
    if (before != expr) {
      val con = targets.nonEmpty
      if (con) before.changed -/-> this
      ref() = expr
      if (con) {
        expr.changed ---> this
        val beforeV = before.value
        val exprV   = expr.value
        fire(Change(beforeV, exprV))
      }
    }
  }

  final def transform(f: Ex => Ex)(implicit tx: S#Tx): Unit = this() = f(this())

  final def value(implicit tx: S#Tx): A = ref().value

  private[lucre] final def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[A]] =
    if (pull.parents(this /* select() */).isEmpty) {
      Some(pull.resolve[Change[A]])
    } else {
      pull(this().changed)
    }

  override def toString = "Expr.Var" + id
}

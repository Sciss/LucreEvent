/*
 *  Type.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package expr

import de.sciss.lucre.{event => evt}
import evt.Sys
import serial.{DataInput, DataOutput}

trait Type[A] extends TypeLike[A, ({type λ[~ <: Sys[~]] = Expr[~, A]})#λ] {
  final protected type Ex [S <: Sys[S]] = Expr[S, A]
  final protected type ExN[S <: Sys[S]] = Expr.Node[S, A]
  final protected type Change = model.Change[A]

  // ---- abstract ----

  def readValue(in: DataInput): A
  def writeValue(value: A, out: DataOutput): Unit

  // ---- public ----

  final def newConst[S <: Sys[S]](value: A): Expr.Const[S, A] = new Const(value)

  final def newVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): Expr.Var[S, A] = {
    val targets = evt.Targets.partial[S]
    val ref = tx.newPartialVar[Ex[S]](targets.id, init)
    new Var(ref, targets)
  }

  final def newConfluentVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): Expr.Var[S, A] = {
    val targets = evt.Targets[S]
    val ref = tx.newVar[Ex[S]](targets.id, init)
    new Var(ref, targets)
  }

  final def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A] = {
    val cookie = in.readByte()
    require(cookie == 3, "Unexpected cookie " + cookie) // XXX TODO cookie should be available in lucre.event
    newConst[S](readValue(in))
  }

  final def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Expr.Var[S, A] = {
    val targets = evt.Targets.read[S](in, access)
    val cookie = in.readByte()
    require(cookie == 0, "Unexpected cookie " + cookie)
    val ref = if (targets.isPartial) {
      tx.readPartialVar[Ex[S]](targets.id, in)
    } else {
      tx.readVar[Ex[S]](targets.id, in)
    }
    new Var(ref, targets)
  }

  final protected def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                              (implicit tx: S#Tx): ReprVar[S] with evt.Node[S] = {
    val ref = if (targets.isPartial) {
      tx.readPartialVar[Ex[S]](targets.id, in)
    } else {
      tx.readVar[Ex[S]](targets.id, in)
    }
    new Var(ref, targets)
  }

  // ---- private ----

  private final case class Const[S <: Sys[S]](constValue: A) extends expr.impl.ConstImpl[S, A] {
    // def react(fun: S#Tx => Change[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = evt.Observer.dummy[S]

    protected def writeData(out: DataOutput): Unit =
      writeValue(constValue, out)
  }

  private final class Var[S <: Sys[S]](protected val ref: S#Var[Ex[S]], protected val targets: evt.Targets[S])
    extends impl.VarImpl[S, A] {
    def reader: event.Reader[S, Ex[S]] = serializer[S]
  }
}
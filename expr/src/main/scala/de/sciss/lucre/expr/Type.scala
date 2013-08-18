/*
 *  Type.scala
 *  (LucreExpr)
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

package de.sciss
package lucre
package expr

import de.sciss.lucre.{event => evt}
import serial.{DataInput, DataOutput}

trait Type[A] extends TypeLike[A, ({type λ[~ <: stm.Sys[~]] = Expr[~, A]})#λ] {
  final protected type Ex [S <: stm.Sys[S]] = Expr[S, A]
  final protected type ExN[S <: stm.Sys[S]] = Expr[S, A] with event.Node[S]
  final protected type Change = model.Change[A]

  // ---- abstract ----

  def readValue(in: DataInput): A
  def writeValue(value: A, out: DataOutput): Unit

  // ---- public ----

  final def newConst[S <: stm.Sys[S]](value: A): Expr.Const[S, A] = new Const(value)

  final def newVar[S <: evt.Sys[S]](init: Ex[S])(implicit tx: S#Tx): Expr.Var[S, A] = {
    val targets = evt.Targets.partial[S]
    val ref = tx.newPartialVar[Ex[S]](targets.id, init)
    new Var(ref, targets)
  }

  final def newConfluentVar[S <: evt.Sys[S]](init: Ex[S])(implicit tx: S#Tx): Expr.Var[S, A] = {
    val targets = evt.Targets[S]
    val ref = tx.newVar[Ex[S]](targets.id, init)
    new Var(ref, targets)
  }

  final def readConst[S <: stm.Sys[S]](in: DataInput): Expr.Const[S, A] = {
    val cookie = in.readByte()
    require(cookie == 3, "Unexpected cookie " + cookie) // XXX TODO cookie should be available in lucre.event
    newConst[S](readValue(in))
  }

  final def readVar[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Expr.Var[S, A] = {
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

  final protected def readVar[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                              (implicit tx: S#Tx): ReprVar[S] with evt.Node[S] = {
    val ref = if (targets.isPartial) {
      tx.readPartialVar[Ex[S]](targets.id, in)
    } else {
      tx.readVar[Ex[S]](targets.id, in)
    }
    new Var(ref, targets)
  }

  // ---- private ----

  private final case class Const[S <: stm.Sys[S]](constValue: A) extends expr.impl.ConstImpl[S, A] {
    // def react(fun: S#Tx => Change[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = evt.Observer.dummy[S]

    protected def writeData(out: DataOutput): Unit =
      writeValue(constValue, out)
  }

  private final class Var[S <: evt.Sys[S]](protected val ref: S#Var[Ex[S]], protected val targets: evt.Targets[S])
    extends impl.VarImpl[S, A] {
    def reader: event.Reader[S, Ex[S]] = serializer[S]
  }
}
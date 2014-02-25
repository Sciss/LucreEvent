/*
 *  TypeLike.scala
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

import lucre.{event => evt}
import evt.Sys
import serial.{DataInput, DataOutput, Serializer}
import language.higherKinds

trait TypeLike[A, Repr[S <: Sys[S]] <: Expr[S, A]] {
  protected type ReprVar  [S <: Sys[S]] = Repr[S] with Expr.Var[S, A]
  protected type ReprConst[S <: Sys[S]] = Repr[S] with Expr.Const[S, A]
  protected type ReprNode [S <: Sys[S]] = Repr[S] with evt.Node[S]

  // ---- abstract ----

  def readValue(in: DataInput): A

  def writeValue(value: A, out: DataOutput): Unit

  // ---- public ----

  def newConst[S <: Sys[S]](value: A): Repr[S] with Expr.Const[S, A]

  def newVar[S <: Sys[S]](init: Repr[S])(implicit tx: S#Tx): ReprVar[S]

  def newConfluentVar[S <: Sys[S]](init: Repr[S])(implicit tx: S#Tx): ReprVar[S]

  def readConst[S <: Sys[S]](in: DataInput): ReprConst[S]

  def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ReprVar[S]

  final def readExpr[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr[S] =
    serializer.read(in, access)

  implicit final def serializer[S <: Sys[S]]: evt.Serializer[S, Repr[S]] /* evt.EventLikeSerializer[S, Repr[S]] */ =
    anySer.asInstanceOf[Ser[S]]

  implicit final def varSerializer[S <: Sys[S]]: evt.Serializer[S, ReprVar[S]] /* Serializer[S#Tx, S#Acc, ReprVar[S]] */ =
    anyVarSer.asInstanceOf[VarSer[S]]

  final def change[S <: Sys[S]](before: A, now: A): Option[model.Change[A]] =
    new model.Change(before, now).toOption

  private val anySer    = new Ser   [evt.InMemory]
  private val anyVarSer = new VarSer[evt.InMemory]

  private final class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, ReprVar[S]] with evt.Reader[S, ReprVar[S]] {
    def write(v: ReprVar[S], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ReprVar[S] = readVar[S](in, access)

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): ReprVar[S] with evt.Node[S] =
      readVar[S](in, access, targets)
  }

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Repr[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): ReprNode[S] = {
      // 0 = var, 1 = op
      in.readByte() match {
        case 0      => readVar(in, access, targets)
        case cookie => readTuple(cookie, in, access, targets)
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Repr[S] = newConst(readValue(in))
  }

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                          (implicit tx: S#Tx): ReprNode[S]

  protected def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                        (implicit tx: S#Tx): ReprVar[S] with evt.Node[S]

  sealed trait TupleOp { def id: Int }

  trait Tuple1Op[T1] extends TupleOp {
    def value(a: T1): A

    final def unapply[S <: Sys[S]](ex: Expr[S, A])(implicit tx: S#Tx): Option[Expr[S, T1]] = ex match {
      case tup: Tuple1[_, _] if tup.op == this => Some(tup._1.asInstanceOf[Expr[S, T1]])
      case _ => None
    }

    def toString[S <: Sys[S]](_1: Expr[S, T1]): String
  }

  final class Tuple1[S <: Sys[S], T1](typeID: Int, val op: Tuple1Op[T1],
                                          protected val targets: evt.Targets[S],
                                          val _1: Expr[S, T1])
    extends impl.NodeImpl[S, A] {

    protected def reader = serializer[S]

    private[lucre] def connect   ()(implicit tx: S#Tx): Unit = _1.changed ---> this
    private[lucre] def disconnect()(implicit tx: S#Tx): Unit = _1.changed -/-> this

    def value(implicit tx: S#Tx) = op.value(_1.value)

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)
      out.writeInt(typeID)
      out.writeInt(op.id)
      _1.write(out)
    }

    private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[model.Change[A]] =
      pull(_1.changed).flatMap { ach =>
        change(op.value(ach.before), op.value(ach.now))
      }

    override def toString = op.toString(_1)
  }

  trait Tuple2Op[T1, T2] extends TupleOp {
    def value(a: T1, b: T2): A

    final protected def writeTypes(out: DataOutput) = ()

    final def unapply[S <: Sys[S]](ex: Expr[S, A])(implicit tx: S#Tx): Option[(Expr[S, T1], Expr[S, T2])] =
      ex match {
        case tup: Tuple2[_, _, _] if tup.op == this =>
          Some((tup._1.asInstanceOf[Expr[S, T1]], tup._2.asInstanceOf[Expr[S, T2]]))
        case _ => None
      }

    def toString[S <: Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String
  }

  final class Tuple2[S <: Sys[S], T1, T2](typeID: Int, val op: Tuple2Op[T1, T2],
                                              protected val targets: evt.Targets[S],
                                              val _1: Expr[S, T1], val _2: Expr[S, T2])
    extends impl.NodeImpl[S, A] {

    protected def reader = serializer[S]

    private[lucre] def connect()(implicit tx: S#Tx): Unit = {
      _1.changed ---> this
      _2.changed ---> this
    }

    private[lucre] def disconnect()(implicit tx: S#Tx): Unit = {
      _1.changed -/-> this
      _2.changed -/-> this
    }

    def value(implicit tx: S#Tx) = op.value(_1.value, _2.value)

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(2)
      out.writeInt(typeID)
      out.writeInt(op.id)
      _1.write(out)
      _2.write(out)
    }

    private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[model.Change[A]] = {
      val _1c = _1.changed
      val _2c = _2.changed

      val _1ch = if (pull.contains(_1c)) pull(_1c) else None
      val _2ch = if (pull.contains(_2c)) pull(_2c) else None

      (_1ch, _2ch) match {
        case (Some(ach), None) =>
          val bv = _2.value
          change(op.value(ach.before, bv), op.value(ach.now, bv))
        case (None, Some(bch)) =>
          val av = _1.value
          change(op.value(av, bch.before), op.value(av, bch.now))
        case (Some(ach), Some(bch)) =>
          change(op.value(ach.before, bch.before), op.value(ach.now, bch.now))
        case _ => None
      }
    }

    override def toString = op.toString(_1, _2)
  }
}
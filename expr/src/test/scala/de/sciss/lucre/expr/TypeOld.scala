/*
 *  TypeOld.scala
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

import event.{EventLikeSerializer, Targets, Pull}
import serial.{DataInput, DataOutput}
import language.implicitConversions

/**
 * IDs:
 * 0 = Byte, 1 = Short, 2 = Int, 3 = Long, 4 = Float, 5 = Double, 6 = Boolean, 7 = Char
 * 8 = String
 */
trait TypeOld[S <: event.Sys[S], A] extends Extensions[S, A] with TupleReader[S, A] {
  tpe =>

  type Ex = Expr[S, A]
  type Var = Expr.Var[S, A]
  type Change = model.Change[A]

  def id: Int

  protected type UnaryOp  = Tuple1Op[A]
  protected type BinaryOp = Tuple2Op[A, A]

  protected def readValue(in: DataInput): A

  protected def writeValue(v: A, out: DataOutput): Unit

  /* protected */
  /* sealed */ trait Basic extends Expr[S, A] {
    final protected def reader = serializer
  }

  implicit object serializer extends EventLikeSerializer[S, Ex] {
    def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Ex with event.Node[S] = {
      // 0 = var, 1 = op
      in.readByte() match {
        case 0 => new VarRead(in, access, targets, tx)
        case arity =>
          val clazz = in.readInt()
          val opID = in.readInt()
          if (clazz == tpe.id) {
            readTuple(arity, opID, in, access, targets)
          } else {
            getExtension(clazz)(tx.peer).readTuple(arity, opID, in, access, targets)
          }
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Ex = new ConstRead(in)
   }

  private final class ConstRead(in: DataInput) extends ConstLike {
    protected val constValue = readValue(in)
  }

  private final class VarRead(in: DataInput, access: S#Acc, protected val targets: Targets[S], tx0: S#Tx)
    extends Basic with impl.VarImpl[S, A] {

    protected val ref = tx0.readVar[Ex](id, in)
   }

   /* protected */ def readExpr( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Ex = serializer.read( in, access )

  private sealed trait ConstLike extends impl.ConstImpl[S, A] {
    //    final def react(fun: S#Tx => Change => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    //      Observer[S, Change, Ex](changed, serializer, fun)
    //    }

    override def hashCode = constValue.##

    override def equals(that: Any) = that match {
      case c: ConstLike => constValue.equals(c.constValue)
      case _ => super.equals(that)
    }

    final protected def writeData(out: DataOutput): Unit = writeValue(constValue, out)
  }

  implicit def Const(init: A): Ex = new ConstLike {
    protected val constValue = init
  }

  def Var(init: Ex)(implicit tx: S#Tx): Var = new Basic with impl.VarImpl[S, A] {
    protected val targets = Targets[S]
    protected val ref     = tx.newVar[Ex](id, init)
  }

  def NamedVar(name: => String, init: Ex)(implicit tx: S#Tx): Var = new Basic with impl.VarImpl[S, A] {
    protected val targets = Targets[S]
    protected val ref     = tx.newVar[Ex](id, init)

    override def toString = name
  }

  def readVar(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Var = {
    // XXX should go somewhere else
    val targets = Targets.read[S](in, access)
    require(in.readByte == 0)
    new VarRead(in, access, targets, tx)
  }

  /* protected */ def change(before: A, now: A): Option[Change] = new model.Change(before, now).toOption

  //   protected def newBinaryOp( op: BinaryOp, a: Ex, b: Ex )( implicit tx: S#Tx ) : Ex = new BinaryOpNew( op, a, b, tx )
  //   protected def newUnaryOp( op: UnaryOp, a: Ex )( implicit tx: S#Tx ) : Ex = new UnaryOpNew( op, a, tx )

  /* protected */ sealed trait TupleOp extends event.Reader[S, Ex] {
    def id: Int
  }

  /* protected */ trait Tuple1Op[T1] extends TupleOp {
    //      final def apply( _1: Expr[ S, T1 ])( implicit tx: S#Tx ) : Ex =
    //         new Tuple1[ T1 ]( this, Targets[ S ], _1 )

    def value(a: T1): A

    def toString(_1: Expr[S, T1]): String
  }

  final /* protected */ class Tuple1[T1](typeID: Int, op: Tuple1Op[T1],
                                         protected val targets: Targets[S],
                                         _1: Expr[S, T1])
    extends Basic with impl.NodeImpl[S, A] {
    //      protected def op: Tuple1Op[ T1 ]
    //      protected def _1: Expr[ S, T1 ]

    def connect   ()(implicit tx: S#Tx): Unit = _1.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = _1.changed -/-> this

    def value(implicit tx: S#Tx) = op.value(_1.value)

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)
      //         out.writeShort( op.id )
      out.writeInt(typeID /* tpe.id */)
      out.writeInt(op.id)
      _1.write(out)
    }

    //      private[lucre] def pull( source: Event[ S, _, _ ], update: Any )( implicit tx: S#Tx ) : Option[ Change ] = {
    //         _1.changed.pull( source, update ).flatMap { ach =>
    //            change( op.value( ach.before ), op.value( ach.now ))
    //         }
    //      }

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change] =
      pull(_1.changed).flatMap { ach =>
        change(op.value(ach.before), op.value(ach.now))
      }

    override def toString = op.toString(_1)
  }

  /* protected */ trait Tuple2Op[T1, T2] extends TupleOp {
    //      final def apply( _1: Expr[ S, T1 ], _2: Expr[ S, T2 ])( implicit tx: S#Tx ) : Ex =
    //         new Tuple2[ T1, T2 ]( this, Targets[ S ], _1, _2 )

    def value(a: T1, b: T2): A

    final protected def writeTypes(out: DataOutput) = ()

    def toString(_1: Expr[S, T1], _2: Expr[S, T2]): String
  }

  final /* protected */ class Tuple2[T1, T2](typeID: Int, op: Tuple2Op[T1, T2],
                                             protected val targets: Targets[S],
                                             _1: Expr[S, T1], _2: Expr[S, T2])
    extends Basic with impl.NodeImpl[S, A] {
    //      protected def op: Tuple1Op[ T1 ]
    //      protected def _1: Expr[ S, T1 ]

    def connect()(implicit tx: S#Tx): Unit = {
      _1.changed ---> this
      _2.changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      _1.changed -/-> this
      _2.changed -/-> this
    }

    def value(implicit tx: S#Tx) = op.value(_1.value, _2.value)

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(2)
      out.writeInt(typeID /* tpe.id */)
      out.writeInt(op.id)
      _1.write(out)
      _2.write(out)
    }

    //      private[lucre] def pull( source: Event[ S, _, _ ], update: Any )( implicit tx: S#Tx ) : Option[ Change ] = {
    //         (_1.changed.pull( source, update ), _2.changed.pull( source, update )) match {
    //            case (None, None)                => None
    //            case (Some( ach ), None )        =>
    //               val bv = _2.value
    //               change( op.value( ach.before, bv ), op.value( ach.now, bv ))
    //            case (None, Some( bch ))         =>
    //               val av = _1.value
    //               change( op.value( av, bch.before ), op.value( av, bch.now ))
    //            case (Some( ach ), Some( bch ))  =>
    //               change( op.value( ach.before, bch.before ), op.value( ach.now, bch.now ))
    //         }
    //      }

    def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Change ] = {
      //         val sources = pull.parents( select() )
      val _1c = _1.changed
      val _2c = _2.changed

      val _1ch = if (pull.contains(_1c)) {
        pull(_1c)
      } else {
        None
      }
      val _2ch = if (pull.contains(_2c)) {
        pull(_2c)
      } else {
        None
      }

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
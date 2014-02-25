package de.sciss.lucre
package expr
package impl

import de.sciss.lucre.{event => evt}
import evt.Sys
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.model
import expr.{Int => _Int}

trait ExprTypeImpl[A] extends ExprType[A] with TypeImpl1[({type Repr[~ <: Sys[~]] = Expr[~, A]})#Repr] {
  final protected type Ex [S <: Sys[S]] = Expr     [S, A]
  final protected type ExN[S <: Sys[S]] = Expr.Node[S, A]
  final protected type ExV[S <: Sys[S]] = Expr.Var [S, A]
  final protected type Change = model.Change[A]

  // ---- abstract ----

  //  protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
  //                                     (implicit tx: S#Tx): ExN[S]

  /** The default implementation reads a type `Int` requiring to match `typeID`, followed by an operator id `Int`
    * which will be resolved using `readOpExtension`.
    */
  protected def readNode[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                         (implicit tx: S#Tx): Ex[S] with evt.Node[S] = {
    val tpe  = in.readInt()
    require(tpe == typeID, s"Invalid type id (found $tpe, required $typeID)")
    val opID = in.readInt()
    readExtension(/* cookie, */ opID, in, access, targets)
  }

  // ---- public ----

  implicit final def serializer[S <: Sys[S]]: evt.Serializer[S, Ex[S]] /* evt.EventLikeSerializer[S, Repr[S]] */ =
    anySer.asInstanceOf[Ser[S]]

  implicit final def varSerializer[S <: Sys[S]]: evt.Serializer[S, ExV[S]] /* Serializer[S#Tx, S#Acc, ReprVar[S]] */ =
    anyVarSer.asInstanceOf[VarSer[S]]

  final def newConst[S <: Sys[S]](value: A): Expr.Const[S, A] = new Const(value)

  final def newVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): ExV[S] = {
    val targets = evt.Targets.partial[S]
    val ref     = tx.newPartialVar[Ex[S]](targets.id, init)
    new Var[S](ref, targets)
  }

  final def newConfluentVar[S <: Sys[S]](init: Ex[S])(implicit tx: S#Tx): ExV[S] = {
    val targets = evt.Targets[S]
    val ref     = tx.newVar[Ex[S]](targets.id, init)
    new Var[S](ref, targets)
  }

  final def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A] = {
    val cookie = in.readByte()
    require(cookie == 3, "Unexpected cookie " + cookie) // XXX TODO cookie should be available in lucre.event
    newConst[S](readValue(in))
  }

  final def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = {
    val targets = evt.Targets.read[S](in, access)
    val cookie = in.readByte()
    require(cookie == 0, "Unexpected cookie " + cookie)
    val ref = if (targets.isPartial) {
      tx.readPartialVar[Ex[S]](targets.id, in)
    } else {
      tx.readVar[Ex[S]](targets.id, in)
    }
    new Var[S](ref, targets)
  }

  final protected def readVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                          (implicit tx: S#Tx): ExV[S] = {
    val ref = if (targets.isPartial) {
      tx.readPartialVar[Ex[S]](targets.id, in)
    } else {
      tx.readVar[Ex[S]](targets.id, in)
    }
    new Var[S](ref, targets)
  }

  // ---- private ----

  private[this] final case class Const[S <: Sys[S]](constValue: A) extends expr.impl.ConstImpl[S, A] {
    // def react(fun: S#Tx => Change[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = evt.Observer.dummy[S]

    protected def writeData(out: DataOutput): Unit = writeValue(constValue, out)
  }

  private[this] final class Var[S <: Sys[S]](protected val ref: S#Var[Ex[S]], protected val targets: evt.Targets[S])
    extends impl.VarImpl[S, A] {
    def reader: evt.Reader[S, Ex[S]] = serializer[S]
  }

  private[this] val anySer    = new Ser   [evt.InMemory]
  private[this] val anyVarSer = new VarSer[evt.InMemory]

  private[this] final class VarSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, ExV[S]] with evt.Reader[S, ExV[S]] {
    def write(v: ExV[S], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ExV[S] = readVar[S](in, access)

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): ExV[S] with evt.Node[S] =
      readVar[S](in, access, targets)
  }

  private[this] final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Ex[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Ex[S] with evt.Node[S] = {
      // 0 = var, 1 = op
      in.readByte() match {
        case 0      => readVar (in, access, targets)
        case cookie => readNode(cookie, in, access, targets)
      }
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Ex[S] = newConst(readValue(in))
  }
}
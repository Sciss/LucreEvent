package de.sciss.lucre.expr

import de.sciss.lucre.{event => evt}
import evt._
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.serial.{Writable, DataInput, DataOutput}

object ThesisExample extends App {
  type S = Durable

  // evt.showLog = true
  val system = Durable(BerkeleyDB.tmp())
  system.step { implicit tx => run() }

  trait Expr extends Writable {
    def value(implicit tx: S#Tx): Int
    def changed: Event[S, Int, Expr]
    def +(that: Int)(implicit tx: S#Tx): Expr
  }

  object Random {
    def apply(min: Int, max: Int)(implicit tx: S#Tx): Random = newRandom(min, max)
  }
  trait Random extends Expr {
    def update()(implicit tx: S#Tx): Unit
  }

  def run()(implicit tx: S#Tx): Unit = {
    val r = Random(0, 10)
    val f = r + 64
    println(r == f)
    f.changed.react { implicit tx =>
      n => println("New value: " + n)
    }
    r.update()
    r.update()
  }

  // ---- impl ----

  def random(min: Int, max: Int): Int = util.Random.nextInt(max - min) + min

  def newRandom(min: Int, max: Int)(implicit tx: S#Tx): Random = {
    val tgt = Targets[S]
    val vr  = tx.newIntVar(tgt.id, random(min, max))
    new RandomImpl(tgt, min, max, vr)
  }

  implicit object ExprSer extends evt.NodeSerializer[S, Expr] {
    def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx) = {
      if (in.readByte() == 0) readRandom(in, access, targets) else readAdd(in, access, targets)
    }

    def readRandom(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx) = {
      val min = in.readInt()
      val max = in.readInt()
      val vr  = tx.readIntVar(targets.id, in)
      new RandomImpl(targets, min, max, vr)
    }

    def readAdd(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx) = {
      val ex  = read(in, access)
      val a   = in.readInt()
      new AddImpl(targets, ex, a)
    }
  }

  trait ExprImpl extends Expr with Node[S] {
    protected def reader = ExprSer
    def +(that: Int)(implicit tx: S#Tx): Expr = {
      val tgt = Targets[S]
      new AddImpl(tgt, this, that)
    }
  }

  class RandomImpl(val targets: Targets[S], min: Int, max: Int, vr: S#Var[Int])
    extends Random with ExprImpl with evt.impl.SingleGenerator[S, Int, Expr] {

    override def toString = s"Random(min = $min, max = $max)$id"

    def update()(implicit tx: S#Tx): Unit = {
      val value = random(min, max)
      vr() = value
      fire(value)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(0)
      out.writeInt(min)
      out.writeInt(max)
      vr.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = vr.dispose()

    def value(implicit tx: S#Tx) = vr()
  }

  class AddImpl(val targets: Targets[S], ex: Expr, a: Int)
    extends ExprImpl with evt.impl.StandaloneLike[S, Int, Expr] {

    override def toString = s"($ex + $a)$id"

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(1)
      ex.write(out)
      out.writeInt(a)
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    def connect   ()(implicit tx: S#Tx): Unit = ex.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = ex.changed -/-> this

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Int] =
      ex.changed.pullUpdate(pull).map(_ + a)

    def value(implicit tx: S#Tx) = ex.value + a

    def changed = this
  }
}
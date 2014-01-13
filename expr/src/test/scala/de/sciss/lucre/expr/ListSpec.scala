package de.sciss.lucre.expr

import de.sciss.lucre.event.{Targets, Sys, Durable}
import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.serial.{DataOutput, DataInput}
import scala.concurrent.stm.TxnLocal
import de.sciss.lucre.stm
import de.sciss.model.Change

class ListSpec extends fixture.FlatSpec with ShouldMatchers {
  final type S = Durable
  final type FixtureParam = Durable

  new Unobserved
  new EarlyObserved
  new LateObserved

  implicit object Ints extends Type[Int] {
    def readValue(in: DataInput) = in.readInt()
    def writeValue(value: Int, out: DataOutput): Unit = out.writeInt(value)
    protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                        (implicit tx: S#Tx) = sys.error(s"Unsupported tuple $cookie")
  }

  final def withFixture(test: OneArgTest): Unit = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      //         val (_, cursor) = system.cursorRoot( _ => () )( tx => _ => tx.newCursor() )
      //         test( cursor )
      test(system)
    }
    finally {
      system.close()
    }
  }

  class Unobserved extends Body {
    def situation = "unobserved state"
    def observationPoint(ll: LL, id: Int)(implicit tx: S#Tx) = ()
  }

  abstract class ObservedLike extends Body {
    val obs = new Observation[S]

    def obs1(ll: LL)(implicit tx: S#Tx): Unit
    def obs2(ll: LL)(implicit tx: S#Tx): Unit

    def update(ll: LL, ch: List.Change[S, Expr[S, Int], Change[Int]]) =
      List.Update[S, Expr[S, Int], Change[Int]](ll, Vector(ch))

    def observationPoint(ll: LL, id: Int)(implicit tx: S#Tx): Unit =
      id match {
        case 1 => obs1(ll)
        case 2 => obs2(ll)
        case 3 =>
          val vr = vh()
          obs.assertEquals(
            update(ll, List.Element(vr, Change(5678, 8765))),
            update(ll, List.Removed(0, cn)),
            update(ll, List.Removed(0, vr)),
            update(ll, List.Added  (0, cn2)),
            update(ll, List.Added  (0, cn2)),
            update(ll, List.Added  (2, cn2)),
            update(ll, List.Added  (3, cn2)),
            update(ll, List.Removed(3, cn2)),
            update(ll, List.Removed(2, cn2)),
            update(ll, List.Removed(1, cn2)),
            update(ll, List.Removed(0, cn2))
          )
          obs.clear()

        case 4 =>
          obs.assertEquals(
            update(ll, List.Added  (0, vh()))
          )
          obs.clear()

        case 5 =>
          obs.assertEquals(
            update(ll, List.Element(vh(), Change(8765, 666)))
          )
          obs.clear()
      }
  }

  class EarlyObserved extends ObservedLike {
    def situation = "early observed state"

    def obs1(ll: LL)(implicit tx: S#Tx): Unit = ll.changed.react(obs.register)

    def obs2(ll: LL)(implicit tx: S#Tx): Unit = {
      obs.assertEquals(
        update(ll, List.Added(0, cn)),
        update(ll, List.Added(1, vh()))
      )
      obs.clear()
    }
  }

  class LateObserved extends ObservedLike {
    def situation = "late observed state"

    def obs1(ll: LL)(implicit tx: S#Tx) = ()

    def obs2(ll: LL)(implicit tx: S#Tx): Unit = ll.changed.react(obs.register)
  }

  abstract class Body {
    type LL = List.Modifiable[S, Expr[S, Int], Change[Int]]

    def situation: String
    def observationPoint(ll: LL, id: Int)(implicit tx: S#Tx): Unit

    protected var cn: Expr[S, Int] = _
    protected var cn2: Expr[S, Int] = _
    protected var vh: stm.Source[S#Tx, Expr.Var[S, Int]] = _

    "A linked list" should s"behave as advertised in $situation" in { cursor =>
      val lh = cursor.step { implicit tx =>
        import Ints.serializer
        val ll = List.Modifiable[S, Expr[S, Int], Change[Int]]
        // XXX TODO: why isn't this constructed automatically?
        implicit val ser = List.Modifiable.serializer[S, Expr[S, Int], Change[Int]]
        tx.newHandle(ll)
      }
      cursor.step { implicit tx =>
        val ll = lh()
        assert(ll.size === 0)
        assert(ll.isEmpty)
        assert(!ll.nonEmpty)
        assert(ll.headOption === None)
        assert(ll.lastOption === None)
      }
      cursor.step { implicit tx =>
        val ll  = lh()
        observationPoint(ll, id = 1)
        cn      = Ints.newConst[S](1234)
        ll.addHead(cn)
        val vr = Ints.newVar[S](Ints.newConst(5678))
        import Ints.varSerializer
        vh = tx.newHandle(vr)
        ll.addLast(vr)
        observationPoint(ll, id = 2)
        assert(ll.size === 2)
        assert(!ll.isEmpty)
        assert(ll.nonEmpty)
        assert(ll.head === cn)
        assert(ll.headOption === Some(cn))
        assert(ll.last === vr)
        assert(ll.lastOption === Some(vr))
        assert(ll(0) === cn)
        assert(ll.get(0) === Some(cn))
        assert(ll(1) === vr)
        assert(ll.get(1) === Some(vr))
        assert(ll.indexOf(cn) === 0)
        assert(ll.indexOf(vr) === 1)
        cn2 = Ints.newConst[S](999)
        assert(ll.indexOf(cn2) === -1)
        assert(ll.iterator.toList === scala.List(cn, vr))

        vr() = Ints.newConst(8765)

        assert(ll.removeHead() === cn)
        assert(ll.removeLast() === vr)
        assert(ll.size === 0)
        assert(ll.isEmpty)
        ll.addHead(cn2)
        ll.addHead(cn2)
        ll.addLast(cn2)
        ll.addLast(cn2)
        assert(ll.size === 4)
        assert(ll.nonEmpty)
        ll.clear()
        assert(ll.size === 0)
        assert(ll.isEmpty)

        observationPoint(ll, id = 3)
        ll.addHead(vr)
        observationPoint(ll, id = 4)
      }

      cursor.step { implicit tx =>
        val ll  = lh()
        val vr  = vh()
        vr() = Ints.newConst(666)
        observationPoint(ll, id = 5)
      }
    }
  }
}
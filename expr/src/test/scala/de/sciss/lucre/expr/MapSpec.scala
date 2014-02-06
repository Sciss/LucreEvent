package de.sciss.lucre.expr

import org.scalatest.{Outcome, Matchers, fixture}
import de.sciss.lucre.event.Durable
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.model.Change
import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

/* To run only this test:

test-only de.sciss.lucre.expr.MapSpec

 */
class MapSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = Durable

  def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "A reactive map" should "behave as advertised" in { cursor =>
    val longs = Longs[S]
    import longs.serializer
    val obs = new Observation[S]
    val map = cursor.step { implicit tx =>
      val _map = Map.Modifiable[S, String, Expr[S, Long], Change[Long]]
      _map.changed.react(obs.register)
      assert(_map.isEmpty)
      assert(!_map.nonEmpty)
      assert(_map.size == 0)
      assert(!_map.contains("AAA"))
      assert(_map.iterator      .toList === Nil)
      assert(_map.keysIterator  .toList === Nil)
      assert(_map.valuesIterator.toList === Nil)
      obs.assertEmpty()
      _map
    }

    def upd(change: Map.Change[S, String, Expr[S, Long], Change[Long]])(implicit tx: S#Tx) =
      Map.Update(map, Vec(change))

    val value = cursor.step { implicit tx =>
      obs.clear()
      val _value  = longs.Var(longs.Const(1234L))
      val vOld    = map.put("AAA", _value)
      assert(vOld.isEmpty)
      assert(!map.isEmpty)
      assert(map.nonEmpty)
      assert(map.size === 1)
      assert(map.contains("AAA"))
      assert(map.iterator      .toList.map { case (k, ex) => (k, ex.value) } === scala.List("AAA" -> 1234L))
      assert(map.keysIterator  .toList === scala.List("AAA"))
      assert(map.valuesIterator.toList.map(_.value) === scala.List(1234L))
      _value() = longs.Const(5678L)
      // obs.print()
      obs.assertEquals(upd(Map.Added("AAA", _value)),
                       upd(Map.Element("AAA", _value, Change(1234L, 5678L))))
      _value
    }

    cursor.step { implicit tx =>
      obs.clear()
      val value2 = longs.Const(0L)
      val value3 = longs.Const(1L)
      val vOld1   = map.put("BBB", value2)
      assert(vOld1.isEmpty)
      value() = longs.Const(9L)
      val vOld2   = map.put("AAA", value3)
      assert(vOld2 === Some(value))
      value() = longs.Const(10L)
      assert(map.size === 2)
      assert(map.contains("AAA"))
      assert(map.contains("BBB"))
      assert(map.iterator      .toList.map { case (k, ex) => (k, ex.value) } .sortBy(_._1) ===
        scala.List("AAA" -> 1L, "BBB" -> 0L))
      assert(map.keysIterator  .toList.sorted === scala.List("AAA", "BBB"))
      assert(map.valuesIterator.toList.map(_.value).sorted === scala.List(0L, 1L))
      obs.assertEquals(upd(Map.Added  ("BBB", value2)),
                       upd(Map.Element("AAA", value, Change(5678L, 9L))),
                       upd(Map.Removed("AAA", value)),
                       upd(Map.Added  ("AAA", value3)))
    }

    cursor.step { implicit x =>
      obs.clear()
      val value2  = longs.Const(0L)
      val value3  = longs.Const(1L)
      val vOld3   = map.remove("AAA")
      assert(vOld3 === Some(value3))
      assert(map.size === 1)
      val vOld4   = map.remove("BBB")
      assert(vOld4 === Some(value2))
      assert(map.size === 0)
      assert(map.isEmpty)
      assert(!map.nonEmpty)
      assert(!map.contains("AAA"))
      assert(!map.contains("BBB"))
      assert(map.iterator      .toList === Nil)
      assert(map.keysIterator  .toList === Nil)
      assert(map.valuesIterator.toList === Nil)
      obs.assertEquals(upd(Map.Removed("AAA", value3)),
                       upd(Map.Removed("BBB", value2)))
    }
  }

  object Key {
    implicit object serializer extends ImmutableSerializer[Key] {
      def read(in: DataInput): Key = Key(in.readInt())

      def write(key: Key, out: DataOutput): Unit = out.writeInt(key.i)
    }
  }
  case class Key(i: Int) {
    override def hashCode = 666
  }

  "A reactive map" should "work under hash collisions" in { cursor =>
    val longs = Longs[S]
    import longs.serializer
    val obs = new Observation[S]

    val map = cursor.step { implicit tx =>
      val _map = Map.Modifiable[S, Key, Expr[S, Long], Change[Long]]
      _map.changed.react(obs.register)
      _map
    }

    def upd(change: Map.Change[S, Key, Expr[S, Long], Change[Long]])(implicit tx: S#Tx) =
      Map.Update(map, Vec(change))

    cursor.step { implicit tx =>
      val c1 = longs.Var(longs.Const(1L))
      val c2 = longs.Var(longs.Const(2L))

      map.put(Key(4), c1)
      val oldV = map.put(Key(3), c2)
      assert(oldV.isEmpty)
      c1() = longs.Const(3L)
      c2() = longs.Const(4L)

      assert(map.size === 2)
      assert(map.keysIterator  .map(_.i    ).toList.sorted === scala.List(3, 4))
      assert(map.valuesIterator.map(_.value).toList.sorted === scala.List(3L, 4L))

      obs.assertEquals(upd(Map.Added(Key(4), c1)),
                       upd(Map.Added(Key(3), c2)),
                       upd(Map.Element(Key(4), c1, Change(1L, 3L))),
                       upd(Map.Element(Key(3), c2, Change(2L, 4L))))
      obs.clear()

      val oldV1 = map.remove(Key(4))
      assert(oldV1 === Some(c1))
      c1() = longs.Const(5L)
      c2() = longs.Const(6L)

      assert(map.size === 1)
      assert(map.keysIterator  .map(_.i    ).toList.sorted === scala.List(3))
      assert(map.valuesIterator.map(_.value).toList.sorted === scala.List(6L))

      obs.assertEquals(upd(Map.Removed(Key(4), c1)),
                       upd(Map.Element(Key(3), c2, Change(4L, 6L))))
    }
  }

  "A reactive map" should "allow serialization" in { cursor =>
    val longs = Longs[S]
    import longs.serializer
    val (mapH0, mapH1) = cursor.step { implicit tx =>
      val _map = Map.Modifiable[S, Int, Expr[S, Long], Change[Long]]
      tx.newHandle(_map) -> tx.newHandle(_map: Map[S, Int, Expr[S, Long], Change[Long]])
    }

    cursor.step { implicit tx =>
      mapH0()
      mapH1()
    }
  }
}

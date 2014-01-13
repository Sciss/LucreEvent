package de.sciss.lucre.expr

import scala.concurrent.stm.TxnLocal
import de.sciss.lucre.event.Sys

final class Observation[S <: Sys[S]] {
  private val seqRef = TxnLocal(init = Vector.empty[Any])

  def register(tx: S#Tx)(upd: Any): Unit =
    seqRef.transform(_ :+ upd)(tx.peer)

  def assertEquals(expected: Any*)(implicit tx: S#Tx): Unit = {
    val ob = seqRef.get(tx.peer)
    assert(ob == expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
      + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
  }

  def clear()(implicit tx: S#Tx): Unit =
    seqRef.set(Vector.empty)(tx.peer)

  def assertEmpty()(implicit tx: S#Tx): Unit =
    assertEquals()

  def print()(implicit tx: S#Tx): Unit =
    println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
}
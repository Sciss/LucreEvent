package de.sciss
package lucre
package expr

import event.{Targets, Node, Sys}
import serial.DataInput

trait TupleReader[S <: Sys[S], A] {
  def readTuple(arity: Int, opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
               (implicit tx: S#Tx): Expr[S, A] with Node[S]
}
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
import language.higherKinds
import expr.{String => _}

object Type {
  trait Extension {
    def name: String

    /** Lowest id of handled operators */
    val opLo : Int
    /** Highest id of handled operators. Note: This value is _inclusive_ */
    val opHi : Int

    override def toString = s"$name [lo = $opLo, hi = $opHi]"
  }

  trait Extension1[+Repr[~ <: Sys[~]]] extends Extension {
    def readExtension[S <: evt.Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                      (implicit tx: S#Tx): Repr[S] with evt.Node[S]
  }

  trait Extension2[+Repr[~ <: Sys[~], _]] extends Extension {
    def readExtension[S <: evt.Sys[S], T1](opID: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                          (implicit tx: S#Tx): Repr[S, T1] with evt.Node[S]
  }

  trait Extension3[+Repr[~ <: Sys[~], _, _]] extends Extension {
    def readExtension[S <: evt.Sys[S], T1, T2](opID: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                          (implicit tx: S#Tx): Repr[S, T1, T2] with evt.Node[S]
  }
}
trait Type {
  def typeID: Int
}

trait Type1[Repr[~ <: Sys[~]]] extends Type {
  /** This method is not thread-safe. We assume extensions are registered upon application start only! */
  def registerExtension(ext: Type.Extension1[Repr]): Unit
}

trait Type2[Repr[~ <: Sys[~], _]] extends Type {
  /** This method is not thread-safe. We assume extensions are registered upon application start only! */
  def registerExtension(ext: Type.Extension2[Repr]): Unit
}

trait Type3[Repr[~ <: Sys[~], _, _]] extends Type {
  /** This method is not thread-safe. We assume extensions are registered upon application start only! */
  def registerExtension(ext: Type.Extension3[Repr]): Unit
}

trait ExprType[A] extends Type1[({type Repr[~ <: Sys[~]] = Expr[~, A]})#Repr] {

  // ---- abstract ----

  def readValue(in: DataInput): A
  def writeValue(value: A, out: DataOutput): Unit

  // ---- public ----

  def newConst[S <: Sys[S]](value: A): Expr.Const[S, A]

  def newVar[S <: Sys[S]](init: Expr[S, A])(implicit tx: S#Tx): Expr.Var[S, A]

  def newConfluentVar[S <: Sys[S]](init: Expr[S, A])(implicit tx: S#Tx): Expr.Var[S, A]

  def readConst[S <: Sys[S]](in: DataInput): Expr.Const[S, A]

  def readVar[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Expr.Var[S, A]
}
/*
 *  package.scala
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

package de.sciss.lucre

import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.event.Sys

package object expr {
  private type ExprTypeA[A] = ExprType[A] with Type1A[({type Repr[~ <: Sys[~]] = Expr[~, A]})#Repr]

  val Int    : ExprTypeA[scala.Int    ] = IntImpl
  val Long   : ExprTypeA[scala.Long   ] = LongImpl
  val Double : ExprTypeA[scala.Double ] = DoubleImpl
  val Boolean: ExprTypeA[scala.Boolean] = BooleanImpl
  val String : ExprTypeA[Predef.String] = StringImpl

  private[this] object IntImpl extends impl.ExprTypeImplA[scala.Int] {
    final val typeID = 2

    def readValue (                  in : DataInput ): scala.Int  = in .readInt()
    def writeValue(value: scala.Int, out: DataOutput): Unit       = out.writeInt(value)
  }

  private[this] object LongImpl extends impl.ExprTypeImplA[scala.Long] {
    final val typeID = 3

    def readValue (                   in : DataInput ): scala.Long  = in .readLong()
    def writeValue(value: scala.Long, out: DataOutput): Unit        = out.writeLong(value)
  }

  private[this] object DoubleImpl extends impl.ExprTypeImplA[scala.Double] {
    final val typeID = 5

    def readValue (                     in : DataInput ): scala.Double  = in .readDouble()
    def writeValue(value: scala.Double, out: DataOutput): Unit          = out.writeDouble(value)
  }

  private[this] object BooleanImpl extends impl.ExprTypeImplA[scala.Boolean] {
    final val typeID = 6

    def readValue (                      in : DataInput ): scala.Boolean  = in .readBoolean()
    def writeValue(value: scala.Boolean, out: DataOutput): Unit           = out.writeBoolean(value)
  }

  private[this] object StringImpl extends impl.ExprTypeImplA[Predef.String] {
    final val typeID = 8

    def readValue (                      in : DataInput ): Predef.String  = in .readUTF()
    def writeValue(value: Predef.String, out: DataOutput): Unit           = out.writeUTF(value)
  }
}
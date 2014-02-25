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

package object expr {
  val Int    : ExprType[scala.Int    ] = IntImpl
  val Long   : ExprType[scala.Long   ] = LongImpl
  val Double : ExprType[scala.Double ] = DoubleImpl
  val Boolean: ExprType[scala.Boolean] = BooleanImpl
  val String : ExprType[Predef.String] = StringImpl

  private[this] object IntImpl extends impl.ExprTypeImpl[scala.Int] {
    final val typeID = 2

    def readValue (                  in : DataInput ): scala.Int  = in .readInt()
    def writeValue(value: scala.Int, out: DataOutput): Unit       = out.writeInt(value)
  }

  private[this] object LongImpl extends impl.ExprTypeImpl[scala.Long] {
    final val typeID = 3

    def readValue (                   in : DataInput ): scala.Long  = in .readLong()
    def writeValue(value: scala.Long, out: DataOutput): Unit        = out.writeLong(value)
  }

  private[this] object DoubleImpl extends impl.ExprTypeImpl[scala.Double] {
    final val typeID = 5

    def readValue (                     in : DataInput ): scala.Double  = in .readDouble()
    def writeValue(value: scala.Double, out: DataOutput): Unit          = out.writeDouble(value)
  }

  private[this] object BooleanImpl extends impl.ExprTypeImpl[scala.Boolean] {
    final val typeID = 6

    def readValue (                      in : DataInput ): scala.Boolean  = in .readBoolean()
    def writeValue(value: scala.Boolean, out: DataOutput): Unit           = out.writeBoolean(value)
  }

  private[this] object StringImpl extends impl.ExprTypeImpl[Predef.String] {
    final val typeID = 8

    def readValue (                      in : DataInput ): Predef.String  = in .readUTF()
    def writeValue(value: Predef.String, out: DataOutput): Unit           = out.writeUTF(value)
  }
}
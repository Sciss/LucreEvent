/*
 *  Longs.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package expr

import annotation.switch
import lucre.{event => evt}
import serial.{DataInput, DataOutput}
import language.implicitConversions

object Longs {
  def apply[S <: evt.Sys[S]]: Longs[S] = new Longs[S]
}

final class Longs[S <: evt.Sys[S]] extends TypeOld[S, Long] {
  tpe =>

  val id = 3

  protected def writeValue(v: Long, out: DataOutput): Unit = out.writeLong(v)

  protected def readValue(in: DataInput): Long = in.readLong()

  //   type Ops = LongOps

  // for a stupid reason scalac doesn't eat A <% Ex
  implicit def longOps[A](ex: A)(implicit view: A => Expr[S, Long]): LongOps = new LongOps(ex)

  final class LongOps private[Longs]( ex: Ex ) {
      def +( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Plus( ex, that )
      def -( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Minus( ex, that )
      def min( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Min( ex, that )
      def max( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Max( ex, that )
      def abs( implicit tx: S#Tx ) : Ex = UnaryOp.Abs( ex )
   }

  def readTuple(arity: Int, opID: Int, in: DataInput, access: S#Acc,
                targets: evt.Targets[S])(implicit tx: S#Tx): Ex with event.Node[S] =
    arity match {
      case 1 => UnaryOp (opID).read(in, access, targets)
      case 2 => BinaryOp(opID).read(in, access, targets)
    }

   private object UnaryOp {
      def apply( id: Int ) : UnaryOp = id match {
         case 0 => Abs
      }

      sealed trait Basic extends UnaryOp {
         final def apply( _1: Ex )( implicit tx: S#Tx ) : Ex =
            new Tuple1( tpe.id, this, evt.Targets[ S ], _1 )

         final def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Ex with event.Node[ S ] = {
            val _1 = readExpr( in, access )
            new Tuple1( tpe.id, this, targets, _1 )
         }
      }

      object Abs extends Basic {
         val id = 0
         def value( in: Long ) = math.abs( in )

         def toString( _1: Ex ) = "abs(" + _1 + ")"
      }
   }

//   protected def binaryOp( id: Int ) = BinaryOp( id )

   private object BinaryOp {
      def apply( id: Int ) : Tuple2Op[ Long, Long ] = (id: @switch) match {
         case 0 => Plus
         case 1 => Minus
         case 2 => Min
         case 3 => Max
      }

      sealed trait Basic extends BinaryOp {
         final def apply( _1: Ex, _2: Ex )( implicit tx: S#Tx ) : Ex =
            new Tuple2( tpe.id, this, evt.Targets[ S ], _1, _2 )

         final def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Ex with event.Node[ S ]= {
            val _1 = readExpr( in, access )
            val _2 = readExpr( in, access )
            new Tuple2( tpe.id, this, targets, _1, _2 )
         }
      }

      object Plus extends Basic  {
         val id = 0
         def value( a: Long, b: Long ) = a + b

         def toString( _1: Ex, _2: Ex ) = "(" + _1 + " + " + _2 + ")"
      }

      object Minus extends Basic {
         val id = 1
         def value( a: Long, b: Long ) = a - b

         def toString( _1: Ex, _2: Ex ) = "(" + _1 + " - " + _2 + ")"
      }

      object Min extends Basic {
         val id = 2
         def value( a: Long, b: Long ) = math.min( a, b )

         def toString( _1: Ex, _2: Ex ) = "min(" + _1 + ", " + _2 + ")"
      }

      object Max extends Basic {
         val id = 3
         def value( a: Long, b: Long ) = math.max( a, b )

         def toString( _1: Ex, _2: Ex ) = "max(" + _1 + ", " + _2 + ")"
      }
   }
}

object LongsTests extends App {
   new LongTests( evt.InMemory() )
}

class LongTests[ S <: event.Sys[ S ] with stm.Cursor[ S ]]( system: S ) {
   val strings = new Longs[ S ]
   import strings._
   import system.{ step => ◊ }

   val s    = ◊ { implicit tx => Var( 33 )}
   val s1   = ◊ { implicit tx => s - 50 }
   val s2   = ◊ { implicit tx => s1.abs }
   val eval = ◊ { implicit tx => s2.value }

   println( "Evaluated: " + eval )

   ◊ { implicit tx => s2.changed.react { _ => (ch: Any) => println( "Observed: " + ch )}}

   ◊ { implicit tx => s() = 22 }
}

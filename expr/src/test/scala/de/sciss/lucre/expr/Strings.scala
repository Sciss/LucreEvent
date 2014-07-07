/*
 *  Strings.scala
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
import stm.Cursor
import serial.{DataInput, DataOutput}
import language.implicitConversions

object Strings {
   def apply[ S <: evt.Sys[ S ]] : Strings[ S ] = new Strings[ S ]
}

final class Strings[ S <: evt.Sys[ S ]] private() extends TypeOld[ S, String ] {
   tpe =>

   val id = 8

   protected def writeValue( v: String, out: DataOutput ): Unit = out.writeUTF( v )
   protected def readValue( in: DataInput ) : String = in.readUTF()
//   type Ops = StringOps

  implicit def stringOps[A](ex: A)(implicit view: A => Expr[S, String]): StringOps = new StringOps(ex)

//   protected def extensions: Extensions[ String ] = Strings

   final class StringOps private[Strings]( ex: Ex ) {
      def append( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Append( ex, that )
      def prepend( that: Ex )( implicit tx: S#Tx ) : Ex = BinaryOp.Prepend( ex, that )
      def reverse( implicit tx: S#Tx ) : Ex = UnaryOp.Reverse( ex )
      def toUpperCase( implicit tx: S#Tx ) : Ex = UnaryOp.Upper( ex )
   }

   def readTuple( arity: Int, opID: Int, in: DataInput, access: S#Acc,
                  targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Ex with event.Node[ S ] = {
      (arity: @switch) match {
         case 1 => UnaryOp(  opID ).read( in, access, targets )
         case 2 => BinaryOp( opID ).read( in, access, targets )
      }
   }

//   protected def readLiteral( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Ex =
//      sys.error( "Strings doesn't define a literal type" )
//
//   protected def unaryOp( id: Int ) = UnaryOp( id )

   private object UnaryOp {
      def apply( id: Int ) : UnaryOp = (id: @switch) match {
         case 0 => Reverse
         case 1 => Upper
      }

      sealed trait Basic extends UnaryOp {
         final def apply( _1: Ex )( implicit tx: S#Tx ) : Ex =
            new Tuple1( tpe.id, this, evt.Targets[ S ], _1 )

         final def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Ex with event.Node[ S ] = {
            val _1 = readExpr( in, access )
            new Tuple1( tpe.id, this, targets, _1 )
         }
      }

      object Reverse extends Basic {
         val id = 0
         def value( in: String ) = in.reverse
         def toString( _1: Ex ) = _1.toString + ".reverse"
      }

      object Upper extends Basic {
         val id = 1
         def value( in: String ) = in.toUpperCase
         def toString( _1: Ex ) = _1.toString + ".toUpperCase"
      }
   }

//   protected def binaryOp( id: Int ) = BinaryOp( id )

   private object BinaryOp {
      def apply( id: Int ) : BinaryOp = (id: @switch) match {
         case 0 => Append
         case 1 => Prepend
      }

      sealed trait Basic extends BinaryOp {
         final def apply( _1: Ex, _2: Ex )( implicit tx: S#Tx ) : Ex =
            new Tuple2( tpe.id, this, evt.Targets[ S ], _1, _2 )

         final def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Ex with event.Node[ S ] = {
            val _1 = readExpr( in, access )
            val _2 = readExpr( in, access )
            new Tuple2( tpe.id, this, targets, _1, _2 )
         }
      }

      object Append extends Basic {
         val id = 0
         def value( a: String, b: String ) = a + b
         def toString( _1: Ex, _2: Ex ) = _1.toString + ".append(" + _2 + ")"
      }

      object Prepend extends Basic {
         val id = 1
         def value( a: String, b: String ) = b + a
         def toString( _1: Ex, _2: Ex ) = _1.toString + ".prepend(" + _2 + ")"
      }
   }
}

object StringsTests extends App {
   new StringTests( evt.InMemory() )
}

class StringTests[ S <: evt.Sys[ S ] with Cursor[ S ]]( system: S ) {
   val strings = Strings[ S ]
   import strings._
   import system.{ step => ◊ }

   val s    = ◊ { implicit tx => Var( "hallo" )}
   val s1   = ◊ { implicit tx => s.append( "-welt" )}
   val s2   = ◊ { implicit tx => s1.reverse }
   val eval = ◊ { implicit tx => s2.value }

   println( "Evaluated: " + eval )

   ◊ { implicit tx => s2.changed.react { _ => (ch: Any) => println( "Observed: " + ch )}}

   ◊ { implicit tx => s() = "kristall".reverse }
}

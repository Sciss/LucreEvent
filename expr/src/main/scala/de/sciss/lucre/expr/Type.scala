/*
 *  Type.scala
 *  (LucreExpr)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package expr

import de.sciss.lucre.{event => evt}
import stm.Serializer

trait Type[ A ] extends TypeLike[ A, ({ type λ[ ~ <: stm.Sys[ ~ ]] = Expr[ ~, A ]})#λ ] {
   final protected type Ex[  S <: stm.Sys[ S ]] = Expr[ S, A ]
   final protected type ExN[ S <: stm.Sys[ S ]] = Expr[ S, A ] with event.Node[ S ]
   final protected type Change[ S <: stm.Sys[ S ]] = event.Change[ A ]

   // ---- abstract ----

   /* protected */ def readValue( in: DataInput ) : A
   /* protected */ def writeValue( value: A, out: DataOutput ) : Unit

   // ---- public ----

   final def newConst[ S <: stm.Sys[ S ]]( value: A ) : Expr.Const[ S, A ] = new Const( value )

   final def newVar[ S <: evt.Sys[ S ]]( init: Ex[ S ])( implicit tx: S#Tx ) : Expr.Var[ S, A ] = {
      val targets = evt.Targets.partial[ S ]
      val ref     = tx.newPartialVar[ Ex[ S ]]( targets.id, init )
      new Var( ref, targets )
   }

   final def newConfluentVar[ S <: evt.Sys[ S ]]( init: Ex[ S ])( implicit tx: S#Tx ) : Expr.Var[ S, A ] = {
      val targets = evt.Targets[ S ]
      val ref     = tx.newVar[ Ex[ S ]]( targets.id, init )
      new Var( ref, targets )
   }

   final def readConst[ S <: stm.Sys[ S ]]( in: DataInput ) : Expr.Const[ S, A ] = {
      val cookie = in.readUnsignedByte()
      require( cookie == 3, "Unexpected cookie " + cookie ) // XXX TODO cookie should be available in lucre.event
      newConst[ S ]( readValue( in ))
   }

   final def readVar[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, A ] = {
      val targets = evt.Targets.read[ S ]( in, access )
      val cookie  = in.readUnsignedByte
      require( cookie == 0, "Unexpected cookie " + cookie )
      val ref     = if( targets.isPartial ) {
         tx.readPartialVar[ Ex[ S ]]( targets.id, in )
      } else {
         tx.readVar[ Ex[ S ]]( targets.id, in )
      }
      new Var( ref, targets )
   }

   final protected def readVar[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])
                                                  ( implicit tx: S#Tx ) : ReprVar[ S ] with evt.Node[ S ] = {

      val ref = if( targets.isPartial ) {
         tx.readPartialVar[ Ex[ S ]]( targets.id, in )
      } else {
         tx.readVar[ Ex[ S ]]( targets.id, in )
      }
      new Var( ref, targets )
   }

//   final def readExpr[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Ex[ S ] =
//      serializer.read( in, access )
//
//   implicit final def serializer[ S <: evt.Sys[ S ]] : evt.EventLikeSerializer[ S, Ex[ S ]] =
//      anySer.asInstanceOf[ Ser[ S ]]
//
//   implicit final def varSerializer[ S <: evt.Sys[ S ]] : Serializer[ S#Tx, S#Acc, Expr.Var[ S, A ]] =
//      anyVarSer.asInstanceOf[ VarSer[ S ]]
//
//   final def change[ S <: stm.Sys[ S ]]( before: A, now: A ) : Option[ Change[ S ]] = new event.Change( before, now ).toOption
//
//   private val anySer      = new Ser[ event.InMemory ]
//   private val anyVarSer   = new VarSer[ event.InMemory ]

//   private final class VarSer[ S <: evt.Sys[ S ]] extends Serializer[ S#Tx, S#Acc, Expr.Var[ S, A ]] {
//      def write( v: Expr.Var[ S, A ], out: DataOutput ) { v.write( out )}
//      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, A ] = readVar[ S ]( in, access )
//   }
//
//   private final class Ser[ S <: evt.Sys[ S ]] extends evt.EventLikeSerializer[ S, Ex[ S ]] {
//      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ExN[ S ] = {
//         // 0 = var, 1 = op
//         (in.readUnsignedByte() /*: @switch */) match {
//            case 0 =>
//               val ref = if( targets.isPartial ) {
//                  tx.readPartialVar[ Ex[ S ]]( targets.id, in )
//               } else {
//                  tx.readVar[ Ex[ S ]]( targets.id, in )
//               }
//               new Var( ref, targets )
//
//            case cookie => readTuple( cookie, in, access, targets )
////               val clazz   = in.readInt()
////               val opID    = in.readInt()
////               if( clazz == tpe.id ) {
////                  readTuple( arity, opID, in, access, targets )
////               } else {
////                  getExtension( clazz )( tx.peer ).readTuple( arity, opID, in, access, targets )
////               }
//         }
//      }
//
//      def readConstant( in: DataInput )( implicit tx: S#Tx ) : Ex[ S ] = newConst( readValue( in ))
//   }

//   protected def readTuple[ S <: evt.Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[ S ])
//                                          ( implicit tx: S#Tx ) : ExN[ S ]
//
//   /* protected */ sealed trait TupleOp /* extends event.Reader[ S, Ex ] */ {
//      def id: Int
//   }
//
//   /* protected */ trait Tuple1Op[ T1 ] extends TupleOp {
////      final def apply( _1: Expr[ S, T1 ])( implicit tx: S#Tx ) : Ex =
////         new Tuple1[ T1 ]( this, Targets[ S ], _1 )
//
//      def value( a: T1 ) : A
//
//      def toString[ S <: evt.Sys[ S ]]( _1: Expr[ S, T1 ]) : String
//   }
//
//   final /* protected */ class Tuple1[ S <: evt.Sys[ S ], T1 ]( typeID: Int, op: Tuple1Op[ T1 ],
//                                       protected val targets: evt.Targets[ S ],
//                                       _1: Expr[ S, T1 ])
//   extends impl.NodeImpl[ S, A ] {
////      protected def op: Tuple1Op[ T1 ]
////      protected def _1: Expr[ S, T1 ]
//
//      protected def reader = serializer[ S ]
//
//      private[lucre] def connect()( implicit tx: S#Tx ) {
//         _1.changed ---> this
//      }
//
//      private[lucre] def disconnect()( implicit tx: S#Tx ) {
//         _1.changed -/-> this
//      }
//
//      def value( implicit tx: S#Tx ) = op.value( _1.value )
//
//      protected def writeData( out: DataOutput ) {
//         out.writeUnsignedByte( 1 )
////         out.writeShort( op.id )
//         out.writeInt( typeID /* tpe.id */)
//         out.writeInt( op.id )
//         _1.write( out )
//      }
//
////      private[lucre] def pull( source: Event[ S, _, _ ], update: Any )( implicit tx: S#Tx ) : Option[ Change ] = {
////         _1.changed.pull( source, update ).flatMap { ach =>
////            change( op.value( ach.before ), op.value( ach.now ))
////         }
////      }
//
//      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Change[ S ]] = {
//         _1.changed.pullUpdate( pull ).flatMap { ach =>
//            change( op.value( ach.before ), op.value( ach.now ))
//         }
//      }
//
//      override def toString = op.toString( _1 )
//   }
//
//   /* protected */  trait Tuple2Op[ T1, T2 ] extends TupleOp {
////      final def apply( _1: Expr[ S, T1 ], _2: Expr[ S, T2 ])( implicit tx: S#Tx ) : Ex =
////         new Tuple2[ T1, T2 ]( this, Targets[ S ], _1, _2 )
//
//      def value( a: T1, b: T2 ) : A
//
//      final protected def writeTypes( out: DataOutput ) {}
//
//      def toString[ S <: stm.Sys[ S ]]( _1: Expr[ S, T1 ], _2: Expr[ S, T2 ]) : String
//   }
//
//   final /* protected */  class Tuple2[ S <: evt.Sys[ S ], T1, T2 ]( typeID: Int, op: Tuple2Op[ T1, T2 ],
//                                           protected val targets: evt.Targets[ S ],
//                                           _1: Expr[ S, T1 ], _2: Expr[ S, T2 ])
//   extends impl.NodeImpl[ S, A ] {
////      protected def op: Tuple1Op[ T1 ]
////      protected def _1: Expr[ S, T1 ]
//
//      protected def reader = serializer[ S ]
//
//      private[lucre] def connect()( implicit tx: S#Tx ) {
//         _1.changed ---> this
//         _2.changed ---> this
//      }
//
//      private[lucre] def disconnect()( implicit tx: S#Tx ) {
//         _1.changed -/-> this
//         _2.changed -/-> this
//      }
//
//      def value( implicit tx: S#Tx ) = op.value( _1.value, _2.value )
//
//      protected def writeData( out: DataOutput ) {
//         out.writeUnsignedByte( 2 )
//         out.writeInt( typeID /* tpe.id */)
//         out.writeInt( op.id )
//         _1.write( out )
//         _2.write( out )
//      }
//
////      private[lucre] def pull( source: Event[ S, _, _ ], update: Any )( implicit tx: S#Tx ) : Option[ Change ] = {
////         (_1.changed.pull( source, update ), _2.changed.pull( source, update )) match {
////            case (None, None)                => None
////            case (Some( ach ), None )        =>
////               val bv = _2.value
////               change( op.value( ach.before, bv ), op.value( ach.now, bv ))
////            case (None, Some( bch ))         =>
////               val av = _1.value
////               change( op.value( av, bch.before ), op.value( av, bch.now ))
////            case (Some( ach ), Some( bch ))  =>
////               change( op.value( ach.before, bch.before ), op.value( ach.now, bch.now ))
////         }
////      }
//
//      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Change[ S ]] = {
////         val sources = pull.parents( select() )
//         val _1c = _1.changed
//         val _2c = _2.changed
//
//         val _1ch = if( _1c.isSource( pull )) {
//            _1c.pullUpdate( pull )
//         } else {
//            None
//         }
//         val _2ch = if( _2c.isSource( pull )) {
//            _2c.pullUpdate( pull )
//         } else {
//            None
//         }
//
//         (_1ch, _2ch) match {
//            case (Some( ach ), None) =>
//               val bv = _2.value
//               change( op.value( ach.before, bv ), op.value( ach.now, bv ))
//            case (None, Some( bch )) =>
//               val av = _1.value
//               change( op.value( av, bch.before ), op.value( av, bch.now ))
//            case (Some( ach ), Some( bch )) =>
//               change( op.value( ach.before, bch.before ), op.value( ach.now, bch.now ))
//            case _ => None
//         }
//      }
//
//      override def toString = op.toString( _1, _2 )
//   }
//
   // ---- private ----

   private final case class Const[ S <: stm.Sys[ S ]]( constValue: A ) extends expr.impl.ConstImpl[ S, A ] {
      def react( fun: S#Tx => Change[ S ] => Unit )( implicit tx: S#Tx ) : evt.Observer[ S, Change[ S ], Ex[ S ]] = {
//         Observer( serializer[ S ], fun )
         evt.Observer.dummy
      }

      protected def writeData( out: DataOutput ) {
         writeValue( constValue, out )
      }
   }

   private final class Var[ S <: evt.Sys[ S ]]( protected val ref: S#Var[ Ex[ S ]], protected val targets: evt.Targets[ S ])
   extends impl.VarImpl[ S, A ] {
      def reader : event.Reader[ S, Ex[ S ]] = serializer[ S ]
   }
}
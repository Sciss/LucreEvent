package de.sciss.lucre
package expr

import de.sciss.lucre.{event => evt}
import stm.Serializer

trait TypeLike[ A, Repr[ S <: stm.Sys[ S ]] <: Expr[ S, A ]] {
   protected type ReprVar[ S <: stm.Sys[ S ]] = Repr[ S ] with Expr.Var[ S, A ]
   protected type ReprConst[ S <: stm.Sys[ S ]] = Repr[ S ] with Expr.Const[ S, A ]
   protected type ReprNode[ S <: stm.Sys[ S ]] = Repr[ S ] with evt.Node[ S ]

   // ---- abstract ----

   def readValue( in: DataInput ) : A
   def writeValue( value: A, out: DataOutput ) : Unit

   // ---- public ----

   def newConst[ S <: stm.Sys[ S ]]( value: A ) : Repr[ S ] with Expr.Const[ S, A ]

   def newVar[ S <: evt.Sys[ S ]]( init: Repr[ S ])( implicit tx: S#Tx ) : ReprVar[ S ]

   def newConfluentVar[ S <: evt.Sys[ S ]]( init: Repr[ S ])( implicit tx: S#Tx ) : ReprVar[ S ]

   def readConst[ S <: stm.Sys[ S ]]( in: DataInput ) : ReprConst[ S ]

   def readVar[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ReprVar[ S ]

   final def readExpr[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Repr[ S ] =
      serializer.read( in, access )

   implicit final def serializer[ S <: evt.Sys[ S ]] : evt.EventLikeSerializer[ S, Repr[ S ]] =
      anySer.asInstanceOf[ Ser[ S ]]

   implicit final def varSerializer[ S <: evt.Sys[ S ]] : Serializer[ S#Tx, S#Acc, ReprVar[ S ]] =
      anyVarSer.asInstanceOf[ VarSer[ S ]]

   final def change[ S <: stm.Sys[ S ]]( before: A, now: A ) : Option[ evt.Change[ A ]] =
      new evt.Change( before, now ).toOption

   private val anySer      = new Ser[ event.InMemory ]
   private val anyVarSer   = new VarSer[ event.InMemory ]

   private final class VarSer[ S <: evt.Sys[ S ]] extends Serializer[ S#Tx, S#Acc, ReprVar[ S ]] {
      def write( v: ReprVar[ S ], out: DataOutput ) { v.write( out )}
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ReprVar[ S ] = readVar[ S ]( in, access )
   }

   private final class Ser[ S <: evt.Sys[ S ]] extends evt.EventLikeSerializer[ S, Repr[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ReprNode[ S ] = {
         // 0 = var, 1 = op
         (in.readUnsignedByte() /*: @switch */) match {
            case 0 =>
//               val ref = if( targets.isPartial ) {
//                  tx.readPartialVar[ Repr[ S ]]( targets.id, in )
//               } else {
//                  tx.readVar[ Repr[ S ]]( targets.id, in )
//               }
//               new Var( ref, targets )
               readVar( in, access, targets )

            case cookie => readTuple( cookie, in, access, targets )
         }
      }

      def readConstant( in: DataInput )( implicit tx: S#Tx ) : Repr[ S ] = newConst( readValue( in ))
   }

   protected def readTuple[ S <: evt.Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[ S ])
                                              ( implicit tx: S#Tx ) : ReprNode[ S ]

   protected def readVar[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])
                                            ( implicit tx: S#Tx ) : ReprVar[ S ] with evt.Node[ S ]

   sealed trait TupleOp {
      def id: Int
   }

   trait Tuple1Op[ T1 ] extends TupleOp {
      def value( a: T1 ) : A
      def toString[ S <: evt.Sys[ S ]]( _1: Expr[ S, T1 ]) : String
   }

   final class Tuple1[ S <: evt.Sys[ S ], T1 ]( typeID: Int, op: Tuple1Op[ T1 ],
                                                protected val targets: evt.Targets[ S ],
                                                _1: Expr[ S, T1 ])
   extends impl.NodeImpl[ S, A ] {
      protected def reader = serializer[ S ]

      private[lucre] def connect()( implicit tx: S#Tx ) {
         _1.changed ---> this
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         _1.changed -/-> this
      }

      def value( implicit tx: S#Tx ) = op.value( _1.value )

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
         out.writeInt( typeID )
         out.writeInt( op.id )
         _1.write( out )
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ evt.Change[ A ]] = {
         _1.changed.pullUpdate( pull ).flatMap { ach =>
            change( op.value( ach.before ), op.value( ach.now ))
         }
      }

      override def toString = op.toString( _1 )
   }

   trait Tuple2Op[ T1, T2 ] extends TupleOp {
      def value( a: T1, b: T2 ) : A

      final protected def writeTypes( out: DataOutput ) {}

      def toString[ S <: stm.Sys[ S ]]( _1: Expr[ S, T1 ], _2: Expr[ S, T2 ]) : String
   }

   final class Tuple2[ S <: evt.Sys[ S ], T1, T2 ]( typeID: Int, op: Tuple2Op[ T1, T2 ],
                                                    protected val targets: evt.Targets[ S ],
                                                    _1: Expr[ S, T1 ], _2: Expr[ S, T2 ])
   extends impl.NodeImpl[ S, A ] {

      protected def reader = serializer[ S ]

      private[lucre] def connect()( implicit tx: S#Tx ) {
         _1.changed ---> this
         _2.changed ---> this
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         _1.changed -/-> this
         _2.changed -/-> this
      }

      def value( implicit tx: S#Tx ) = op.value( _1.value, _2.value )

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 2 )
         out.writeInt( typeID )
         out.writeInt( op.id )
         _1.write( out )
         _2.write( out )
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ evt.Change[ A ]] = {
         val _1c = _1.changed
         val _2c = _2.changed

         val _1ch = if( _1c.isSource( pull )) {
            _1c.pullUpdate( pull )
         } else {
            None
         }
         val _2ch = if( _2c.isSource( pull )) {
            _2c.pullUpdate( pull )
         } else {
            None
         }

         (_1ch, _2ch) match {
            case (Some( ach ), None) =>
               val bv = _2.value
               change( op.value( ach.before, bv ), op.value( ach.now, bv ))
            case (None, Some( bch )) =>
               val av = _1.value
               change( op.value( av, bch.before ), op.value( av, bch.now ))
            case (Some( ach ), Some( bch )) =>
               change( op.value( ach.before, bch.before ), op.value( ach.now, bch.now ))
            case _ => None
         }
      }

      override def toString = op.toString( _1, _2 )
   }
}
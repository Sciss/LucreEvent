/*
 *  ReactionTest.scala
 *  (LucreSTM)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucrestm
package fluent

import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.TMap
import java.awt.{GridLayout, EventQueue}
import javax.swing.{JTextField, BorderFactory, SwingConstants, JLabel, GroupLayout, JPanel, WindowConstants, JFrame}
import annotation.switch
import java.awt.event.{ActionListener, ActionEvent}

object ReactionTest extends App with Runnable {
   EventQueue.invokeLater( this )

   type Tx  = Confluent#Tx
   type Acc = Confluent#Acc

   object Reactor {
      implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, Reactor[ S ]] = new Ser[ S ]

      private final class Ser[ S <: Sys[ S ]] extends TxnSerializer[ S#Tx, S#Acc, Reactor[ S ]] {
         def write( r: Reactor[ S ], out: DataOutput ) { r.write( out )}

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Reactor[ S ] = {
            if( in.readUnsignedByte() == 0 ) {
               new ReactorBranchStubRead[ S ]( in, tx, access )
            } else {
               new ReactorLeaf[ S ]( in.readLong() )
            }
         }
      }

      private final class ReactorBranchStubRead[ S <: Sys[ S ]]( in: DataInput, tx0: S#Tx, acc: S#Acc )
      extends ReactorBranchStub[ S ] {
         val id = tx0.readID( in, acc )
         protected val children = tx0.readVar[ IIdxSeq[ Reactor[ S ]]]( id, in )
      }
   }

   sealed trait Reactor[ S <: Sys[ S ]] extends Writer {
      def propagate()( implicit tx: S#Tx, map: ReactionMap[ S ]) : Unit
   }

   object ReactorLeaf {
      def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx, map: ReactionMap[ S ]) : ReactorLeaf[ S ] = new ReactorLeaf( map.newID() )
   }

   final case class ReactorLeaf[ S <: Sys[ S ]]( id: Long ) extends Reactor[ S ] {
      def write( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
         out.writeLong( id )
      }

      def propagate()( implicit tx: S#Tx, map: ReactionMap[ S ]) {
         map.invoke( this )
      }
   }

   object Observable {
      def deaf[ S <: Sys[ S ]] : Observable[ S ] = new Deaf[ S ]

      private final class Deaf[ S <: Sys[ S ]] extends Observable[ S ] {
         def addReactor(    r: Reactor[ S ])( implicit tx: S#Tx ) {}
         def removeReactor( r: Reactor[ S ])( implicit tx: S#Tx ) {}
      }
   }

   sealed trait Observable[ S <: Sys[ S ]] {
      def addReactor(    r: Reactor[ S ])( implicit tx: S#Tx ) : Unit
      def removeReactor( r: Reactor[ S ])( implicit tx: S#Tx ) : Unit
   }


//   trait Connector[ -Txn ] {
//      def connect()(    implicit tx: Txn ) : Unit
//      def disconnect()( implicit tx: Txn ) : Unit
//   }

   trait ReactorSources[ S <: Sys[ S ]] {
      def reactorSources( implicit tx: S#Tx ) : IIdxSeq[ Observable[ S ]]
   }

//   type ReactorSources[ S <: Sys[ S ]] = IIdxSeq[ Observable[ S ]]

   object ReactorBranch {
      def apply[ S <: Sys[ S ]]( sources: ReactorSources[ S ])( implicit tx: S#Tx ) : ReactorBranch[ S ] =
         new New[ S ]( sources, tx )

      private final class New[ S <: Sys[ S ]]( protected val sources: ReactorSources[ S ], tx0: S#Tx )
      extends ReactorBranch[ S ] {
         val id = tx0.newID()
         protected val children = tx0.newVar[ IIdxSeq[ Reactor[ S ]]]( id, IIdxSeq.empty )
      }

//      def serializer[ S <: Sys[ S ]]: TxnSerializer[ S#Tx, S#Acc, ReactorBranch[ S ]] =
//         new TxnSerializer[ S#Tx, S#Acc, ReactorBranch[ S ]] {
//            def write( r: ReactorBranch[ S ], out: DataOutput ) { r.write( out )}
//            def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ReactorBranch[ S ] = {
//               require( in.readUnsignedByte() == 0 )
//               new ReactorBranchRead( in, access, tx )
//            }
//         }

      def read[ S <: Sys[ S ]]( sources: ReactorSources[ S ], in: DataInput, access: S#Acc )
                              ( implicit tx: S#Tx ) : ReactorBranch[ S ] = {
         require( in.readUnsignedByte() == 0 )
         new Read[ S ]( sources, in, access, tx )
      }

      private final class Read[ S <: Sys[ S ]]( protected val sources: ReactorSources[ S ],
                                                in: DataInput, access: S#Acc, tx0: S#Tx )
      extends ReactorBranch[ S ] {
         val id = tx0.readID( in, access )
         protected val children = tx0.readVar[ IIdxSeq[ Reactor[ S ]]]( id, in )
      }
   }

   sealed trait ReactorBranchStub[ S <: Sys[ S ]] extends Reactor[ S ] {
      def id: S#ID
      protected def children: S#Var[ IIdxSeq[ Reactor[ S ]]]

      final def isConnected( implicit tx: S#Tx ) : Boolean = children.get.nonEmpty

      final def propagate()( implicit tx: S#Tx, map: ReactionMap[ S ]) {
         children.get.foreach( _.propagate() )
      }

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( 0 )
         id.write( out )
         children.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         require( !isConnected )
         id.dispose()
         children.dispose()
      }

      override def toString = "ReactorBranch" + id

      override def equals( that: Any ) : Boolean = {
         (if( that.isInstanceOf[ ReactorBranchStub[ _ ]]) {
            id == that.asInstanceOf[ ReactorBranchStub[ _ ]].id
         } else super.equals( that ))
      }

      override def hashCode = id.hashCode()
   }

   /**
    * A `ReactorBranch` is most similar to EScala's `EventNode` class. It represents an observable
    * object and can also act as an observer itself.
    */
   sealed trait ReactorBranch[ S <: Sys[ S ]] extends ReactorBranchStub[ S ] with Observable[ S ] /* with Mutable[ S ] */ {
      protected def sources: ReactorSources[ S ]

//      protected def connect()( implicit tx: S#Tx ) : Unit
//      // note: 'undeploy' is a rather horrible neologism (only used by Apache Tomcat and with airbags...)
//      protected def disconnect()( implicit tx: S#Tx ) : Unit

      final def addReactor( r: Reactor[ S ])( implicit tx: S#Tx ) {
         val old = children.get
         children.set( old :+ r )
         if( old.isEmpty ) {
//            connector.connect()
            sources.reactorSources.foreach( _.addReactor( this ))
         }
      }

      final def removeReactor( r: Reactor[ S ])( implicit tx: S#Tx ) {
         val xs = children.get
         val i = xs.indexOf( r )
         if( i >= 0 ) {
            val xs1 = xs.patch( i, IIdxSeq.empty, 1 ) // XXX crappy way of removing a single element
            children.set( xs1 )
            if( xs1.isEmpty ) {
//               connector.disconnect()
               sources.reactorSources.foreach( _.removeReactor( this ))
            }
         }
      }
   }

   trait Event[ S <: Sys[ S ], A ] extends Observable[ S ] with Writer {
//      protected def reactor: ReactorBranch[ S ]
      protected def reactor: Observable[ S ]
      def value( implicit tx: S#Tx ) : A

      final def addReactor(    r: Reactor[ S ])( implicit tx: S#Tx ) { reactor.addReactor(    r )}
      final def removeReactor( r: Reactor[ S ])( implicit tx: S#Tx ) { reactor.removeReactor( r )}
   }

   object ReactionMap {
      def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : ReactionMap[ S ] = new Impl[ S ]( tx )

      private final class Impl[ S <: Sys[ S ]]( tx0: S#Tx ) extends ReactionMap[ S ] {
         private val map   = TMap.empty[ ReactorLeaf[ S ], S#Tx => Unit ]
         val id            = tx0.newID()
         private val cnt   = tx0.newLongVar( id, 0L )

         def invoke( key: ReactorLeaf[ S ])( implicit tx: S#Tx ) {
            map.get( key )( tx.peer ).foreach( fun =>
              fun.apply( tx )
            )
         }

         def add( key: ReactorLeaf[ S ], fun: S#Tx => Unit )( implicit tx: S#Tx ) {
            map.+=( key -> fun )( tx.peer )
         }

         def remove( key: ReactorLeaf[ S ])( implicit tx: S#Tx ) {
            map.-=( key )( tx.peer )
         }

         def newID()( implicit tx: S#Tx ) : Long = {
            val res = cnt.get
            cnt.set( res + 1 )
            res
         }
      }
   }
   sealed trait ReactionMap[ S <: Sys[ S ]] {
      def newID()( implicit tx: S#Tx ) : Long
      def add(    key: ReactorLeaf[ S ], reaction: S#Tx => Unit )( implicit tx: S#Tx ) : Unit
      def remove( key: ReactorLeaf[ S ])( implicit tx: S#Tx ) : Unit
      def invoke( key: ReactorLeaf[ S ])( implicit tx: S#Tx ) : Unit
   }

   sealed trait Observer { def remove()( implicit tx: Tx ) : Unit }

   sealed trait Expr[ A ] extends Event[ Confluent, A ] with Disposable[ Tx ] {
      def observe( update: A => Unit )( implicit tx: Tx, map: ReactionMap[ Confluent ]) : Observer = {
         val key        = ReactorLeaf.apply[ Confluent ]()
         val reaction   = (tx: Tx) => {
            val now = value( tx )
            update( now )
         }
         map.add( key, reaction )
         addReactor( key )
         val res = new Observer {
            def remove()( implicit tx: Tx ) {
               map.remove( key )
               removeReactor( key )
            }
         }
         update( value( tx ))
         res
      }
   }

   trait ConstExpr[ A ] extends Expr[ A ] {
      final protected val reactor = Observable.deaf[ Confluent ]
      protected def constValue : A
      final def value( implicit tx: Tx ) : A = constValue
      final def dispose()( implicit tx: Tx ) {}
   }

   trait MutableExpr[ A ] extends Expr[ A ] {
      protected def reactor: ReactorBranch[ Confluent ]
   }

   trait BinaryExpr[ A ] extends MutableExpr[ A ] {
      protected def a: Expr[ A ]
      protected def b: Expr[ A ]
      protected def op( a: A, b: A ) : A

      final def value( implicit tx: Tx ) : A = op( a.value, b.value )

      final def dispose()( implicit tx: Tx ) {
         a.removeReactor( reactor )
         b.removeReactor( reactor )
         reactor.dispose()
      }
   }

   object ExprVar {
//      def apply[ A, Ex <: Expr[ A ]]( init: Ex )( implicit tx: Tx, ser: TxnSerializer[ Tx, Acc, Ex ]) : ExprVar[ Ex ] =
//         new ExprVarNew[ A, Ex ]( init, tx )

      sealed trait Impl[ A, Ex <: Expr[ A ]] extends ExprVar[ A, Ex ] {
         protected implicit def peerSer: TxnSerializer[ Tx, Acc, Ex ]
         protected implicit def reactionMap: ReactionMap[ Confluent ]
         protected def id: Confluent#ID
         protected def v: Confluent#Var[ Ex ]
         protected def reactor: ReactorBranch[ Confluent ]

         final def get( implicit tx: Tx ) : Ex = v.get
         final def set( ex: Ex )( implicit tx: Tx ) {
            val old = get
            if( old.value != ex.value ) {
               old.removeReactor( reactor )
               v.set( ex )
               ex.addReactor( reactor )
               reactor.propagate()
            }
         }

         final def transform( fun: Ex => Ex )( implicit tx: Tx ) {
//            v.transform( fun )
            set( fun( get ))
         }

         final def write( out: DataOutput ) {
            out.writeUnsignedByte( 100 )
            id.write( out )
            v.write( out )
            reactor.write( out )
         }

         final def dispose()( implicit tx: Tx ) {
            id.dispose()
            v.dispose()
            reactor.dispose()
         }
      }

      // XXX the other option is to forget about StringRef, LongRef, etc., and instead
      // pimp Expr[ String ] to StringExprOps, etc.
      class New[ A, Ex <: Expr[ A ]]( init: Ex, tx0: Tx )(
         implicit protected val peerSer: TxnSerializer[ Tx, Acc, Ex ],
         implicit protected val reactionMap: ReactionMap[ Confluent ])
      extends Impl[ A, Ex ] {
         val id                  = tx0.newID()
         protected val v         = tx0.newVar[ Ex ]( id, init )
         private val sources : ReactorSources[ Confluent ] = new ReactorSources[ Confluent ] {
            def reactorSources( implicit tx: Tx ) = IIdxSeq( v.get )
         }
         protected val reactor   = ReactorBranch[ Confluent ]( sources )( tx0 )
//         init.addReactor( reactor )( tx0 )
      }

      class Read[ A, Ex <: Expr[ A ]]( in: DataInput, access: Acc, tx0: Tx )(
         implicit protected val peerSer: TxnSerializer[ Tx, Acc, Ex ],
         implicit protected val reactionMap: ReactionMap[ Confluent ])
      extends Impl[ A, Ex ] {
         val id                  = tx0.readID( in, access )
         protected val v         = tx0.readVar[ Ex ]( id, in )
         private val sources : ReactorSources[ Confluent ] = new ReactorSources[ Confluent ] {
            def reactorSources( implicit tx: Tx ) = IIdxSeq( v.get )
         }
         protected val reactor   = ReactorBranch.read[ Confluent ]( sources, in, access )( tx0 )
//         init.addReactor( reactor )
      }
   }
   trait ExprVar[ A, Ex <: Expr[ A ]] extends /* Expr[ Ex ] with */ Var[ Tx, Ex ] with Writer {
      final def value( implicit tx: Tx ) : A = get.value
   }

   object StringRef {
      implicit def apply( s: String )( implicit tx: Tx ) : StringRef = new StringConstNew( s, tx )

      def append( a: StringRef, b: StringRef )( implicit tx: Tx ) : StringRef =
         new StringAppendNew( a, b, tx )

      private sealed trait StringConst extends StringRef with ConstExpr[ String ] {
         final def write( out: DataOutput ) {
            out.writeUnsignedByte( 0 )
            out.writeString( constValue )
         }

         override def toString = constValue
      }

      private final class StringConstNew( protected val constValue: String, tx0: Tx )
      extends StringConst

      private final class StringConstRead( in: DataInput, tx0: Tx ) extends StringConst {
         protected val constValue: String = in.readString()
      }

      private sealed trait StringBinOp extends StringRef with BinaryExpr[ String ] {
         protected def opID : Int

         final def write( out: DataOutput ) {
            out.writeUnsignedByte( opID )
            a.write( out )
            b.write( out )
            reactor.write( out )
         }

         final protected val sources : ReactorSources[ Confluent ] = new ReactorSources[ Confluent ] {
            def reactorSources( implicit t: Tx ) = IIdxSeq( a, b )
         }
      }

      private sealed trait StringAppend {
         me: BinaryExpr[ String ] =>

         final protected def op( ac: String, bc: String ) : String = ac + bc
         final protected def opID = 1

         override def toString = "String.append(" + a + ", " + b + ")"
      }

      private final class StringAppendNew( protected val a: StringRef, protected val b: StringRef, tx0: Tx )
      extends StringBinOp with StringAppend {
         protected val reactor = ReactorBranch[ Confluent ]( sources )( tx0 )
//         a.addReactor( reactor )( tx0 )
//         b.addReactor( reactor )( tx0 )
      }

      private final class StringAppendRead( in: DataInput, access: Acc, tx0: Tx )( implicit map: ReactionMap[ Confluent ])
      extends StringBinOp with StringAppend {
         protected val a         = ser.read( in, access )( tx0 )
         protected val b         = ser.read( in, access )( tx0 )
         protected val reactor   = ReactorBranch.read[ Confluent ]( sources, in, access )( tx0 )
//         a.addReactor( reactor )( tx0 )
//         b.addReactor( reactor )( tx0 )
      }

      implicit def ser( implicit map: ReactionMap[ Confluent ]) : TxnSerializer[ Tx, Acc, StringRef ] =
         new TxnSerializer[ Tx, Acc, StringRef ] {
            def read( in: DataInput, access: Acc )( implicit tx: Tx ) : StringRef = {
               (in.readUnsignedByte(): @switch) match {
                  case 0   => new StringConstRead( in, tx )
                  case 1   => new StringAppendRead( in, access, tx )
                  case 100 => new ExprVar.Read[ String, StringRef ]( in, access, tx ) with StringRef {
                     override def toString = "String.ref(" + v + ")"
                  }
               }
            }

            def write( v: StringRef, out: DataOutput ) { v.write( out )}
         }
   }

   trait StringRef extends Expr[ String ] {
      final def append( other: StringRef )( implicit tx: Tx ) : StringRef = StringRef.append( this, other )
   }

   object LongRef {
      implicit def apply( n: Long )( implicit tx: Tx ) : LongRef = new LongConstNew( n, tx )

      def plus( a: LongRef, b: LongRef )( implicit tx: Tx ) : LongRef = new LongPlusNew( a, b, tx )
      def min(  a: LongRef, b: LongRef )( implicit tx: Tx ) : LongRef = new LongMinNew(  a, b, tx )
      def max(  a: LongRef, b: LongRef )( implicit tx: Tx ) : LongRef = new LongMaxNew(  a, b, tx )

      private sealed trait LongConst extends LongRef with ConstExpr[ Long ] {
         final def write( out: DataOutput ) {
            out.writeUnsignedByte( 0 )
            out.writeLong( constValue )
         }
      }

      private final class LongConstNew( protected val constValue: Long, tx0: Tx )
      extends LongConst

      private final class LongConstRead( in: DataInput, tx0: Tx )
      extends LongConst {
         protected val constValue: Long = in.readLong()
      }

      private sealed trait LongBinOp extends LongRef with BinaryExpr[ Long ] {
         protected def opID: Int

         final protected val sources : ReactorSources[ Confluent ] = new ReactorSources[ Confluent ] {
            def reactorSources( implicit tx: Tx ) = IIdxSeq( a, b )
         }

         final def write( out: DataOutput ) {
            out.writeUnsignedByte( opID )
            a.write( out )
            b.write( out )
            reactor.write( out )
         }

//         final protected def connect()( implicit tx: Tx ) {
//            a.addReactor( reactor )
//            b.addReactor( reactor )
//         }
//
//         final protected def disconnect()( implicit tx: Tx ) {
//            a.removeReactor( reactor )
//            b.removeReactor( reactor )
//         }
      }

      private abstract class LongBinOpNew( tx0: Tx ) extends LongBinOp {
         final protected val reactor = ReactorBranch[ Confluent ]( sources )( tx0 )
      }

      private abstract class LongBinOpRead( in: DataInput, access: Acc, tx0: Tx )( implicit map: ReactionMap[ Confluent ])
      extends LongBinOp {
         final protected val a         = ser.read( in, access )( tx0 )
         final protected val b         = ser.read( in, access )( tx0 )
         final protected val reactor   = ReactorBranch.read[ Confluent ]( sources, in, access )( tx0 )
      }

      private sealed trait LongPlus {
         final protected def op( ac: Long, bc: Long ) = ac + bc
         final protected def opID = 1
      }

      private sealed trait LongMin {
         final protected def op( ac: Long, bc: Long ) = math.min( ac, bc )
         final protected def opID = 2
      }

      private sealed trait LongMax extends LongRef with BinaryExpr[ Long ] {
         final protected def op( ac: Long, bc: Long ) = math.max( ac, bc )
         final protected def opID = 3
      }

      private final class LongPlusNew( protected val a: LongRef, protected val b: LongRef, tx0: Tx )
      extends LongBinOpNew( tx0 ) with LongPlus

      private final class LongPlusRead( in: DataInput, access: Acc, tx0: Tx )( implicit map: ReactionMap[ Confluent ])
      extends LongBinOpRead( in, access, tx0 ) with LongPlus

      private final class LongMinNew( protected val a: LongRef, protected val b: LongRef, tx0: Tx )
      extends LongBinOpNew( tx0 ) with LongMin

      private final class LongMinRead( in: DataInput, access: Acc, tx0: Tx )( implicit map: ReactionMap[ Confluent ])
      extends LongBinOpRead( in, access, tx0 ) with LongMin

      private final class LongMaxNew( protected val a: LongRef, protected val b: LongRef, tx0: Tx )
      extends LongBinOpNew( tx0 ) with LongMax

      private final class LongMaxRead( in: DataInput, access: Acc, tx0: Tx )( implicit map: ReactionMap[ Confluent ])
      extends LongBinOpRead( in, access, tx0 ) with LongMax

      implicit def ser( implicit map: ReactionMap[ Confluent ]) : TxnSerializer[ Tx, Acc, LongRef ] =
         new TxnSerializer[ Tx, Acc, LongRef ] {
            def read( in: DataInput, access: Acc )( implicit tx: Tx ) : LongRef = {
               (in.readUnsignedByte(): @switch) match {
                  case 0   => new LongConstRead( in, tx )
                  case 1   => new LongPlusRead( in, access, tx )
                  case 2   => new LongMinRead( in, access, tx )
                  case 3   => new LongMaxRead( in, access, tx )
                  case 100 => new ExprVar.Read[ Long, LongRef ]( in, access, tx ) with LongRef
               }
            }

            def write( v: LongRef, out: DataOutput ) { v.write( out )}
         }
   }

   trait LongRef extends Expr[ Long ] {
      final def +(   other: LongRef )( implicit tx: Tx ) : LongRef = LongRef.plus( this, other )
      final def min( other: LongRef )( implicit tx: Tx ) : LongRef = LongRef.min(  this, other )
      final def max( other: LongRef )( implicit tx: Tx ) : LongRef = LongRef.max(  this, other )
   }

//   object Region {
//      def apply( name: StringRef, start: LongRef, stop: LongRef ) : Region = new RegionNew( name, start, stop )
//
//      private final class RegionNew( name0: StringRef, start0: LongRef, stop: LongRef ) extends Region {
//
//      }
//   }


   object Region {
      def apply( name: StringRef, start: LongRef, stop: LongRef )
               ( implicit tx: Tx, map: ReactionMap[ Confluent ]) : Region =
         new RegionNew( name, start, stop, tx )

      private final class RegionNew( name0: StringRef, start0: LongRef, stop0: LongRef, tx0: Tx )
                                   ( implicit map: ReactionMap[ Confluent ])
      extends Region {
         region =>

         val id = tx0.newID()

//         private val nameRef = tx0.newVar[ StringRef ]( id, name0 )
         val name_# = new ExprVar.New[ String, StringRef ]( name0, tx0 ) with StringRef {
            override def toString = region.toString + ".name_#"
         }
         def name( implicit tx: Tx ) : StringRef = name_#.get
         def name_=( value: StringRef )( implicit tx: Tx ) { name_#.set( value )}

//         private val startRef = tx0.newVar[ LongRef ]( id, start0 )
         val start_# = new ExprVar.New[ Long, LongRef ]( start0, tx0 ) with LongRef
         def start( implicit tx: Tx ) : LongRef = start_#.get
         def start_=( value: LongRef )( implicit tx: Tx ) { start_#.set( value )}

//         private val stopRef = tx0.newVar[ LongRef ]( id, stop0 )
         val stop_# = new ExprVar.New[ Long, LongRef ]( stop0, tx0 ) with LongRef
         def stop( implicit tx: Tx ) : LongRef = stop_#.get
         def stop_=( value: LongRef )( implicit tx: Tx ) { stop_#.set( value )}

         override def toString = "Region" + id
      }
   }

   trait Region {
      def name( implicit tx: Tx ) : StringRef
      def name_=( value: StringRef )( implicit tx: Tx ) : Unit
      def name_# : StringRef

      def start( implicit tx: Tx ) : LongRef
      def start_=( value: LongRef )( implicit tx: Tx ) : Unit
      def start_# : LongRef

      def stop( implicit tx: Tx ) : LongRef
      def stop_=( value: LongRef )( implicit tx: Tx ) : Unit
      def stop_# : LongRef
   }

   trait RegionList {
      def head( implicit tx: Tx ) : Option[ List[ Region ]]
      def head_=( r: Option[ List[ Region ]]) : Unit
   }

   trait List[ A ] {
      def value: A
      def next( implicit tx: Tx ) : Option[ List[ A ]]
//      def next_=( elem: Option[ List[ A ]])( implicit tx: Tx ) : Unit
   }

   final class RegionView( r: Region, id: String ) extends JPanel {
      private val lay = new GroupLayout( this )
      lay.setAutoCreateContainerGaps( true )
      setLayout( lay )
      setBorder( BorderFactory.createTitledBorder( BorderFactory.createEtchedBorder(), id ))

      private val lbName   = new JLabel( "Name:", SwingConstants.RIGHT )
      private val lbStart  = new JLabel( "Start:", SwingConstants.RIGHT )
      private val lbStop   = new JLabel( "Stop:", SwingConstants.RIGHT )

      private val ggName   = new JTextField( 12 )
      private val ggStart  = new JTextField( 8 )
      private val ggStop   = new JTextField( 8 )

      lay.setHorizontalGroup( lay.createSequentialGroup()
         .addGroup( lay.createParallelGroup()
            .addComponent( lbName )
            .addComponent( lbStart )
            .addComponent( lbStop )
         )
         .addGroup( lay.createParallelGroup()
            .addComponent( ggName )
            .addComponent( ggStart )
            .addComponent( ggStop )
         )
      )

      lay.setVerticalGroup( lay.createSequentialGroup()
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbName )
            .addComponent( ggName )
         )
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbStart )
            .addComponent( ggStart )
         )
         .addGroup( lay.createParallelGroup( GroupLayout.Alignment.BASELINE )
            .addComponent( lbStop )
            .addComponent( ggStop )
         )
      )

      private def stringToModel( s: String, model: (Tx, String) => Unit )( implicit system: Confluent ) {
         system.atomic { implicit tx =>
            model( tx, s )
         }
      }

      private def longToModel( n: Long, model: (Tx, Long) => Unit )( implicit system: Confluent ) {
         system.atomic { implicit tx => model( tx, n )}
      }

      def connect()( implicit tx: Tx, map: ReactionMap[ Confluent ]) {
         r.name_#.observe(  v => defer( ggName.setText(  v )))
         r.start_#.observe( v => defer( ggStart.setText( v.toString )))
         r.stop_#.observe(  v => defer( ggStop.setText(  v.toString )))

         implicit val system = tx.system

         ggName.addActionListener( new ActionListener {
            def actionPerformed( e: ActionEvent ) {
               stringToModel( ggName.getText, (tx, s) => { implicit val _tx = tx; r.name = s })
            }
         })

         ggStart.addActionListener( new ActionListener {
            def actionPerformed( e: ActionEvent ) {
               longToModel( ggStart.getText.toLong, (tx, n) => { implicit val _tx = tx; r.start = n })
            }
         })

         ggStop.addActionListener( new ActionListener {
            def actionPerformed( e: ActionEvent ) {
               longToModel( ggStop.getText.toLong, (tx, n) => { implicit val _tx = tx; r.stop = n })
            }
         })
      }
   }

   def defer( thunk: => Unit ) { EventQueue.invokeLater( new Runnable { def run() { thunk }})}

   def run() {
      val system = Confluent()
      implicit val map = system.atomic( tx => ReactionMap.apply[ Confluent ]()( tx ))

      val f    = new JFrame( "Reaction Test" )
      val cp   = f.getContentPane

      cp.setLayout( new GridLayout( 3, 1 ))
      val rs = system.atomic { implicit tx =>
         val _r1   = Region( "eins", 0L, 10000L )
         val _r2   = Region( "zwei", 5000L, 12000L )
         val _r3   = Region( _r1.name_#.append( "+" ).append( _r2.name_# ),
                             _r1.start_#.min( _r2.start_# ).+( -100L ),
                             _r1.stop_#.max( _r2.stop_# ).+( 100L ))
         Seq( _r1, _r2, _r3 )
      }

      val vs = rs.zipWithIndex.map {
         case (r, i) => new RegionView( r, "Region #" + (i+1) )
      }

      system.atomic { implicit tx =>
         vs.foreach( _.connect() )
      }

      vs.foreach( cp.add( _ ))

      f.setResizable( false )
      f.pack()
      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
      f.setLocationRelativeTo( null )
      f.setVisible( true )
   }
}
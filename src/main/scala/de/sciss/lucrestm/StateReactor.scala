/*
 *  StateReactor.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}

object StateReactor {
   implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, StateReactor[ S ]] = new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]] extends TxnSerializer[ S#Tx, S#Acc, StateReactor[ S ]] {
      def write( r: StateReactor[ S ], out: DataOutput ) { r.write( out )}

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : StateReactor[ S ] = {
         if( in.readUnsignedByte() == 0 ) {
            val id            = tx.readID( in, access )
   //         val children   = tx.readVar[ IIdxSeq[ StateReactor[ S ]]]( id, in )
            val children      = tx.readVar[ IIdxSeq[ StateReactor[ S ]]]( id, in )
            val targets       = StateTargets[ S ]( id, children )
            val observerKeys  = children.get.collect {
               case Key( key ) => key
            }
            tx.mapStateTargets( in, targets, observerKeys )
         } else {
            val key  = in.readInt()
            new Key[ S ]( key )
         }
      }
   }

   final case class Key[ S <: Sys[ S ]] private[lucrestm] ( key: Int ) extends StateReactor[ S ] {
      private[lucrestm] def propagate( reactions: State.Reactions )( implicit tx: S#Tx ) : State.Reactions = reactions
      private[lucrestm] def propagateState( state: State[ S, _, _ ], reactions: State.Reactions )
                                          ( implicit tx: S#Tx ) : State.Reactions =
         tx.propagateState( key, state, reactions )

      def dispose()( implicit tx: S#Tx ) {}

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 1 )
         out.writeInt( key )
      }
   }
}

sealed trait StateReactor[ S <: Sys[ S ]] extends Writer with Disposable[ S#Tx ] {
   private[lucrestm] def propagate( reactions: State.Reactions )( implicit tx: S#Tx ) : State.Reactions
   private[lucrestm] def propagateState( state: State[ S, _, _ ], reactions: State.Reactions )
                                       ( implicit tx: S#Tx ) : State.Reactions
}

object StateObserver {
   def apply[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, Repr <: State[ S, A, Repr ]](
      reader: StateReader[ S, Repr ], fun: (S#Tx, A) => Unit )( implicit tx: S#Tx ) : StateObserver[ S, A, Repr ] = {

      val key = tx.addStateReaction[ A, Repr ]( reader, fun )
      new Impl[ S, A, Repr ]( key )
   }

   private final class Impl[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, Repr <: State[ S, A, Repr ]](
      key: StateReactor.Key[ S ])
   extends StateObserver[ S, A, Repr ] {
      def add( state: Repr )( implicit tx: S#Tx ) {
//         state.addObserver( this )
         state.addReactor( key )
      }
      def remove( state: Repr )( implicit tx: S#Tx ) {
//         state.removeObserver( this )
         state.removeReactor( key )
      }

      def dispose()( implicit tx: S#Tx ) {
         tx.removeStateReaction( key )
      }
   }
}
sealed trait StateObserver[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, -Repr ] extends Disposable[ S#Tx ] {
   def add(    state: Repr )( implicit tx: S#Tx ) : Unit
   def remove( state: Repr )( implicit tx: S#Tx ) : Unit
}

object State {
   type Reaction  = () => () => Unit
   type Reactions = IIdxSeq[ Reaction ]
}

sealed trait State[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, Repr <: State[ S, A, Repr ]] extends Writer {
//   me: Repr =>

   private[lucrestm] def addReactor(     r: StateReactor[ S ])( implicit tx: S#Tx ) : Unit
   private[lucrestm] def removeReactor(  r: StateReactor[ S ])( implicit tx: S#Tx ) : Unit

   def value( implicit tx: S#Tx ) : A

   def observe( fun: (S#Tx, A) => Unit )( implicit tx: S#Tx ) : StateObserver[ S, A, Repr ]
}

trait StateConstant[ S <: Sys[ S ], A, Repr <: StateConstant[ S, A, Repr ]] extends State[ S, A, Repr ] {
//   protected def reader: StateReader[ S, Repr ]

   protected def constValue : A

   final def value( implicit tx: S#Tx ) : A = constValue

   final def observe( fun: (S#Tx, A) => Unit )( implicit tx: S#Tx, ev: Repr <:< this.type ) : StateObserver[ S, A, Repr ] = {
      val o = StateObserver( StateReader.unsupported[ S, Repr ], fun )
      // this is a no-op anyways:
//      o.add( this )
      o
   }

   final private[lucrestm] def addReactor(     r: StateReactor[ S ])( implicit tx: S#Tx ) {}
   final private[lucrestm] def removeReactor(  r: StateReactor[ S ])( implicit tx: S#Tx ) {}
}

//trait StateVar[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, Repr <: StateVar[ S, A, Repr ]]
//extends State[ S, A, Repr ] /* with Var[ S#Tx, A ] */ {
////   me: Repr =>
//   def value
//}

object StateReader {
   def unsupported[ S <: Sys[ S ], Repr ] : StateReader[ S, Repr ] = new Unsupported[ S, Repr ]

   private final class Unsupported[ S <: Sys[ S ], Repr ] extends StateReader[ S, Repr ] {
      def read( in: DataInput, targets: StateTargets[ S ])( implicit tx: S#Tx ) : Repr =
         throw new UnsupportedOperationException()
   }
}

trait StateReader[ S <: Sys[ S ], +Repr ] {
   def read( in: DataInput, targets: StateTargets[ S ])( implicit tx: S#Tx ) : Repr
}

object StateSources {
   def none[ S <: Sys[ S ]] : StateSources[ S ] = new NoSources[ S ]

   private final class NoSources[ S <: Sys[ S ]] extends StateSources[ S ] {
      def stateSources( implicit tx: S#Tx ) : IIdxSeq[ State[ S, _, _ ]] = IIdxSeq.empty
   }
}

trait StateSources[ S <: Sys[ S ]] {
   def stateSources( implicit tx: S#Tx ) : IIdxSeq[ State[ S, _, _ ]]
}

object StateTargets {
   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : StateTargets[ S ] = {
      val id         = tx.newID()
      val children   = tx.newVar[ IIdxSeq[ StateReactor[ S ]]]( id, IIdxSeq.empty )
      new Impl( id, children )
   }

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : StateTargets[ S ] = {
      val id            = tx.readID( in, access )
      val children      = tx.readVar[ IIdxSeq[ StateReactor[ S ]]]( id, in )
      new Impl[ S ]( id, children )
   }

   private[lucrestm] def apply[ S <: Sys[ S ]]( id: S#ID, children: S#Var[ IIdxSeq[ StateReactor[ S ]]]) : StateTargets[ S ] =
      new Impl( id, children )

   private final class Impl[ S <: Sys[ S ]](
      private[lucrestm] val id: S#ID, children: S#Var[ IIdxSeq[ StateReactor[ S ]]])
   extends StateTargets[ S ] {
      private[lucrestm] def propagate( reactions: State.Reactions )( implicit tx: S#Tx ) : State.Reactions = {
         children.get.foldLeft( reactions )( (rs, r) => r.propagate( rs ))
      }

      private[lucrestm] def propagateState( state: State[ S, _, _ ], reactions: State.Reactions )
                                          ( implicit tx: S#Tx ) : State.Reactions = {
         children.get.foldLeft( reactions )( (rs, r) => r.propagateState( state, rs ))
      }

      private[lucrestm] def addReactor( r: StateReactor[ S ])( implicit tx: S#Tx ) : Boolean = {
         val old = children.get
         children.set( old :+ r )
         old.isEmpty
      }

      private[lucrestm] def removeReactor( r: StateReactor[ S ])( implicit tx: S#Tx ) : Boolean = {
         val xs = children.get
         val i = xs.indexOf( r )
         if( i >= 0 ) {
            val xs1 = xs.patch( i, IIdxSeq.empty, 1 ) // XXX crappy way of removing a single element
            children.set( xs1 )
            xs1.isEmpty
         } else false
      }

      def write( out: DataOutput ) {
         out.writeUnsignedByte( 0 )
         id.write( out )
         children.write( out )
      }

      def dispose()( implicit tx: S#Tx ) {
         require( children.get.isEmpty, "Disposing a state reactor which is still being observed" )
         id.dispose()
         children.dispose()
      }
   }
}

sealed trait StateTargets[ S <: Sys[ S ]] extends StateReactor[ S ] {
   private[lucrestm] def id: S#ID
   private[lucrestm] def addReactor(    r: StateReactor[ S ])( implicit tx: S#Tx ) : Boolean
   private[lucrestm] def removeReactor( r: StateReactor[ S ])( implicit tx: S#Tx ) : Boolean
}

/**
 * A `StateNode` is most similar to EScala's `EventNode` class. It represents an observable
 * object and can also act as an observer itself.
 */
/* sealed */ trait StateNode[ S <: Sys[ S ], /* @specialized SUCKAZZZ */ A, Repr <: StateNode[ S, A, Repr ]]
extends StateReactor[ S ] with State[ S, A, Repr ] {
   protected def reader: StateReader[ S, Repr ]
   protected def sources: StateSources[ S ]
   protected def targets: StateTargets[ S ]
   protected def writeData( out: DataOutput ) : Unit
   protected def disposeData()( implicit tx: S#Tx ) : Unit

   final def id: S#ID = targets.id

   final private[lucrestm] def propagate( reactions: State.Reactions )( implicit tx: S#Tx ) : State.Reactions =
      targets.propagateState( this, reactions )

   final private[lucrestm] def propagateState( parent: State[ S, _, _ ], reactions: State.Reactions )
                                             ( implicit tx: S#Tx ) : State.Reactions =
      targets.propagateState( this, reactions ) // parent state not important

   final def write( out: DataOutput ) {
      targets.write( out )
      writeData( out )
   }

   final def dispose()( implicit tx: S#Tx ) {
      targets.dispose()
      disposeData()
   }

   final private[lucrestm] def addReactor( r: StateReactor[ S ])( implicit tx: S#Tx ) {
      if( targets.addReactor( r )) {
         sources.stateSources.foreach( _.addReactor( this ))
      }
   }

   final private[lucrestm] def removeReactor( r: StateReactor[ S ])( implicit tx: S#Tx ) {
      if( targets.removeReactor( r )) {
         sources.stateSources.foreach( _.removeReactor( this ))
      }
   }

   final def observe( fun: (S#Tx, A) => Unit )( implicit tx: S#Tx ) : StateObserver[ S, A, Repr ] = {
      val o = StateObserver( reader, fun )
//      o.add( this: Repr )
      addObserver( o )
      o
   }

   protected def addObserver( o: StateObserver[ S, A, Repr ])( implicit tx: S#Tx ) : Unit

   override def toString = "StateNode" + id

   override def equals( that: Any ) : Boolean = {
      (if( that.isInstanceOf[ StateNode[ _, _, _ ]]) {
         id == that.asInstanceOf[ StateNode[ _, _, _ ]].id
      } else super.equals( that ))
   }

   override def hashCode = id.hashCode()
}

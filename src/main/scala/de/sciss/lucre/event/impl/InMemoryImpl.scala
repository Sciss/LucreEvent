/*
 *  InMemoryImpl.scala
 *  (LucreEvent)
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
package event
package impl

import concurrent.stm.{Ref, InTxn}
import stm.impl.{InMemoryImpl => STMImpl}

object InMemoryImpl {
   private type S = InMemory

   def apply() : InMemory = new System

   private def opNotSupported( name: String ) : Nothing = sys.error( "Operation not supported: " + name )

   private sealed trait BasicVar[ S <: Sys[ S ], @specialized( Int ) A ]
   extends Var[ S, A ] {
      override def toString = "event.Var<" + hashCode().toHexString + ">"

      final def write( out: DataOutput ) {}
      final def isFresh( implicit tx: S#Tx ) : Boolean = true
   }

   private final class VarImpl[ S <: Sys[ S ], A ]( peer: Ref[ A ])
   extends BasicVar[ S, A ] {
      def get( implicit tx: S#Tx ) : Option[ A ] = Option( peer.get( tx.peer ))

      def getOrElse( default: => A )( implicit tx: S#Tx ) : A = {
         val v = peer.get( tx.peer )
         if( v == null ) default else v
      }

      def transform( default: => A )( f: A => A )( implicit tx: S#Tx ) {
         peer.transform( v => f( if( v == null ) default else v ))( tx.peer )
      }

      def set( v: A )( implicit tx: S#Tx ) { peer.set( v )( tx.peer )}

      def dispose()( implicit tx: S#Tx ) {
         peer.set( null.asInstanceOf[ A ])( tx.peer )
      }
   }

   private final class IntVarImpl[ S <: Sys[ S ]]( peer: Ref[ Long ])
   extends BasicVar[ S, Int ] {
      def get( implicit tx: S#Tx ) : Option[ Int ] = {
         val v = peer.get( tx.peer )
         if( v < 0 ) None else Some( v.toInt )
      }

      def getOrElse( default: => Int )( implicit tx: S#Tx ) : Int = {
         val v = peer.get( tx.peer )
         if( v < 0 ) default else v.toInt
      }

      def transform( default: => Int )( f: Int => Int )( implicit tx: S#Tx ) {
         peer.transform( v => f( if( v < 0 ) default else v.toInt ))( tx.peer )
      }

      def set( v: Int )( implicit tx: S#Tx ) { peer.set( v.toLong & 0xFFFFFFFFL )( tx.peer )}

      def dispose()( implicit tx: S#Tx ) {
         peer.set( -1L )( tx.peer )
      }
   }

   trait TxnMixin[ S <: Sys[ S ]] extends Txn[ S ] {
      final private[event] def reactionMap : ReactionMap[ S ] = system.reactionMap

      final private[event] def newEventVar[ A ]( id: S#ID )
                                               ( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ] = {
         new VarImpl( Ref.make[ A ])
      }

      final private[event] def newEventIntVar[ A ]( id: S#ID ) : Var[ S, Int ] = {
         new IntVarImpl( Ref( -1L ))
      }

      final private[event] def readEventVar[ A ]( id: S#ID, in: DataInput )
                                                ( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ] = {
         opNotSupported( "readEventVar" )
      }

      final private[event] def readEventIntVar[ A ]( id: S#ID, in: DataInput ) : Var[ S, Int ] = {
         opNotSupported( "readEventIntVar" )
      }
   }

   private final class TxnImpl( val system: S, val peer: InTxn )
   extends STMImpl.TxnMixin[ S ] with TxnMixin[ S ] {
      override def toString = "event.InMemory#Tx@" + hashCode.toHexString

      def inMemory : InMemory#Tx = this
   }

   private final class System extends STMImpl.Mixin[ S ] with InMemory with ReactionMapImpl.Mixin[ S ] {
      def wrap( peer: InTxn ) : S#Tx = new TxnImpl( this, peer )
      override def toString = "event.InMemory@" + hashCode.toHexString
   }
}

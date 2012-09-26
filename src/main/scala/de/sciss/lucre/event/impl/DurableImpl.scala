package de.sciss.lucre
package event
package impl

import stm.{DataStoreFactory, DataStore}
import concurrent.stm.InTxn

object DurableImpl {
   def ??? : Nothing = sys.error( "TODO" )

   object InMemoryEvents {
      def apply( store: DataStore ) : Durable = ???

      def apply( factory: DataStoreFactory[ DataStore ], name: String ) : Durable =
         apply( factory.open( name ))
   }

   def apply( mainStore: DataStore, eventStore: DataStore ) : Durable = ???

   def apply( factory: DataStoreFactory[ DataStore ], mainName: String, eventName: String ) : Durable =
      apply( factory.open( mainName ), factory.open( eventName ))

   private type D[ S <: DurableLike[ S ]] = DurableLike[ S ]

   private sealed trait DurableSource[ S <: D[ S ], @specialized( Int ) A ] extends event.Var[ S, A ] {
      protected def id: Int

      final def write( out: DataOutput ) {
         out.writeInt( id )
      }

      final def dispose()( implicit tx: S#Tx ) {
         tx.system.removeEvent( id )
      }

      final def getOrElse( default: => A )( implicit tx: S#Tx ) : A = get.getOrElse( default )

      final def transform( default: => A )( f: A => A )( implicit tx: S#Tx ) { set( f( getOrElse( default )))}

      final def isFresh( implicit tx: S#Tx ) : Boolean = true
   }

   private final class DurableVarImpl[ S <: D[ S ], A ]( protected val id: Int,
                                                  protected val ser: stm.Serializer[ S#Tx, S#Acc, A ])
   extends DurableSource[ S, A ] {
      def get( implicit tx: S#Tx ) : Option[ A ] = tx.system.tryReadEvent[ A ]( id )( ser.read( _, () ))

      def set( v: A )( implicit tx: S#Tx ) {
         tx.system.writeEvent( id )( ser.write( v, _ ))
      }

      override def toString = "event.Var(" + id + ")"
   }

   private final class DurableIntVar[ S <: D[ S ]]( protected val id: Int )
   extends DurableSource[ S, Int ] {
      def get( implicit tx: S#Tx ) : Option[ Int ] = {
         tx.system.tryReadEvent[ Int ]( id )( _.readInt() )
      }

      def set( v: Int )( implicit tx: S#Tx ) {
         tx.system.writeEvent( id )( _.writeInt( v ))
      }

      override def toString = "event.Var[Int](" + id + ")"
   }

//   trait Mixin[ S <: D[ S ], I <: Sys[ S ]] extends  {
////      override val inMemory: InMemory = InMemory()
//   }

   trait DurableMixin[ S <: D[ S ] /* , I <: Sys[ I ] */] extends /* stm.impl.DurableImpl.Mixin[ S, I ] with */ DurableLike[ S ] {
      private[event] def tryReadEvent[ A ]( id: Int )( valueFun: DataInput => A )( implicit tx: S#Tx ): Option[ A ] = {
         ???
      }
      private[event] def writeEvent( id: Int )( valueFun: DataOutput => Unit )( implicit tx: S#Tx ) {
         ???
      }
      private[event] def removeEvent( id: Int )( implicit tx: S#Tx ) {
         ???
      }
      private[event] def newEventIDValue()( implicit tx: S#Tx ) : Int = {
         ???
      }
   }

   trait DurableTxnMixin[ S <: D[ S ]] extends DurableLike.Txn[ S ] {
      _: S#Tx =>

      final private[event] def reactionMap : ReactionMap[ S ] = system.reactionMap

      final private[event] def newEventVar[ A ]( id: S#ID )
                                               ( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ] = {
//         new VarImpl( Ref.make[ A ])
         new DurableVarImpl[ S, A ]( system.newEventIDValue()( this ), serializer )
      }

      final private[event] def newEventIntVar[ A ]( id: S#ID ) : Var[ S, Int ] = {
         new DurableIntVar[ S ]( system.newEventIDValue()( this ))
      }

      final private[event] def readEventVar[ A ]( id: S#ID, in: DataInput )
                                                ( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ] = {
         val id = in.readInt()
         new DurableVarImpl[ S, A ]( id, serializer )
      }

      final private[event] def readEventIntVar[ A ]( id: S#ID, in: DataInput ) : Var[ S, Int ] = {
         val id = in.readInt()
         new DurableIntVar[ S ]( id )
      }
   }

   private final class TxnImpl( val system: DurableSystem, val peer: InTxn )
   extends stm.impl.DurableImpl.TxnMixin[ Durable ] with DurableTxnMixin[ Durable ] {
      lazy val inMemory: InMemory#Tx = system.inMemory.wrap( peer )
      override def toString = "event.Durable#Tx@" + hashCode.toHexString
   }

   private final class DurableSystem( protected val store: DataStore )
   extends stm.impl.DurableImpl.Mixin[ Durable, InMemory ]
   with DurableMixin[ Durable] with Durable
   with ReactionMapImpl.Mixin[ Durable ] {
      private type S = Durable
      val inMemory: InMemory = InMemory()
      def wrap( peer: InTxn ) : S#Tx = new TxnImpl( this, peer )
      override def toString = "event.Durable@" + hashCode.toHexString
   }
}

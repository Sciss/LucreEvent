/*
 *  Node.scala
 *  (LucreSTM)
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import stm.Mutable
import annotation.switch
import LucreSTM.logEvent

/**
 * An abstract trait uniting invariant and mutating readers.
 */
/* sealed */ trait Reader[ S <: stm.Sys[ S ], +Repr ] {
   def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Repr with Node[ S ]
}

trait NodeSerializer[ S <: EventSys[ S ], Repr <: Node[ S ]]
extends Reader[ S, Repr ] with stm.Serializer[ S#Tx, S#Acc, Repr ] {
   final def write( v: Repr, out: DataOutput ) { v.write( out )}

   final def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Repr = {
      val targets = Targets.read[ S ]( in, access )
      read( in, access, targets )
   }
}

object Targets {
//   private type I = InMemory
//
//   private implicit def childrenSer[ S <: EventSys[ S ]] : stm.Serializer[ S#Tx, S#Acc, Children[ S ]] =
//      anyChildrenSer.asInstanceOf[ stm.Serializer[ S#Tx, S#Acc, Children[ S ]]]
//
//   private val anyChildrenSer = stm.Serializer.indexedSeq[ I#Tx, I#Acc, (Int, Selector[ I ])](
//      stm.Serializer.tuple2[ I#Tx, I#Acc, Int, Selector[ I ]](
//         stm.Serializer.Int, Selector.serializer[ I ]
//      )
//   )

   def apply[ S <: EventSys[ S ]]( implicit tx: S#Tx ) : Targets[ S ] = {
      val id         = tx.newID()
//      val children   = tx.newVar[ Children[ S ]]( id, NoChildren )
//      val invalid    = tx.newIntVar( id, 0 )
      val children   = tx.newEventVar[ Children[ S ]]( id )
      val invalid    = tx.newEventIntVar( id )
      new Impl( 0, id, children, invalid )
   }

   def partial[ S <: EventSys[ S ]]( implicit tx: S#Tx ) : Targets[ S ] = {
apply[ S ]
//      val id         = tx.newPartialID()
//      val children   = tx.newPartialVar[ Children[ S ]]( id, NoChildren )
//      val invalid    = tx.newIntVar( id, 0 ) // XXX should this be removed? or partial?
//      new Impl( 1, id, children, invalid )
   }

   /* private[lucre] */ def read[ S <: EventSys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Targets[ S ] = {
      (in.readUnsignedByte(): @switch) match {
         case 0      => readIdentified( in, access )
         case 1      => readIdentifiedPartial( in, access )
         case cookie => sys.error( "Unexpected cookie " + cookie )
      }
   }

   private[event] def readIdentified[ S <: EventSys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Targets[ S ] = {
      val id            = tx.readID( in, access )
//      val children      = tx.readVar[ Children[ S ]]( id, in )
//      val invalid       = tx.readIntVar( id, in )
      val children      = tx.readEventVar[ Children[ S ]]( id, in )
      val invalid       = tx.readEventIntVar( id, in )
      new Impl[ S ]( 0, id, children, invalid )
   }

   private[event] def readIdentifiedPartial[ S <: EventSys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Targets[ S ] = {
readIdentified( in, access )
//      val id            = tx.readPartialID( in, access )
//      val children      = tx.readPartialVar[ Children[ S ]]( id, in )
//      val invalid       = tx.readIntVar( id, in )
//      new Impl[ S ]( 1, id, children, invalid )
   }

//   private[event] def apply[ S <: Sys[ S ]]( id: S#ID, children: S#Var[ Children[ S ]]) : Targets[ S ] =
//      new EventImpl( id, children )

   private final class Impl[ S <: EventSys[ S ]]( cookie: Int,
      val id: S#ID, childrenVar: EventVar[ S, Children[ S ]], invalidVar: EventVar[ S, Int ])
   extends Targets[ S ] {
      def write( out: DataOutput ) {
         out.writeUnsignedByte( cookie )
         id.write( out )
         childrenVar.write( out )
         invalidVar.write( out )
      }

      private[lucre] def isPartial : Boolean = cookie == 1

      def dispose()( implicit tx: S#Tx ) {
         require( children.isEmpty, "Disposing a event reactor which is still being observed" )
         id.dispose()
         childrenVar.dispose()
         invalidVar.dispose()
      }

//      def select( slot: Int, invariant: Boolean ) : VirtualNodeSelector[ S ] = Selector( slot, this, invariant )

      private[event] def children( implicit tx: S#Tx ) : Children[ S ] = childrenVar.getOrElse( NoChildren )

      override def toString = "Targets" + id

      private[event] def add( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) : Boolean = {
         logEvent( this.toString + " add( " + slot + ", " + sel + ")" )
         val tup  = (slot, sel)
         val old  = childrenVar.get // .getFresh
         logEvent( this.toString + " old children = " + old )
// MMM
//         sel.writeValue()
         old match {
            case Some( seq ) =>
               childrenVar.set( seq :+ tup )
               seq.exists( _._1 == slot )
            case _ =>
               childrenVar.set( IIdxSeq( tup ))
               false
         }
//
//         childrenVar.set( old :+ tup )
//         !old.exists( _._1 == slot )
      }

      private[event] def resetAndValidate( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) {
         logEvent( this.toString + " resetAndValidate( " + slot + ", " + sel + ")" )
         val tup  = (slot, sel)
// MMM
//         sel.writeValue()
         val old  = if( isPartial ) childrenVar.getOrElse( NoChildren[ S ]) else NoChildren[ S ]
         childrenVar.set( old :+ tup )
         validated( slot )
      }

      private[event] def remove( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) : Boolean = {
         logEvent( this.toString + " remove( " + slot + ", " + sel + ")" )
         val tup  = (slot, sel)
         val xs   = childrenVar.getOrElse( NoChildren )
         logEvent( this.toString + " old children = " + xs )
         val i    = xs.indexOf( tup )
         if( i >= 0 ) {
            val xs1 = xs.patch( i, IIdxSeq.empty, 1 ) // XXX crappy way of removing a single element
            childrenVar.set( xs1 )
   //         xs1.isEmpty
            !xs1.exists( _._1 == slot )
         } else {
            logEvent( this.toString + " selector not found" )
            false
         }
      }

      private[event] def observers( implicit tx: S#Tx ): IIdxSeq[ ObserverKey[ S ]] =
         children.flatMap( _._2.toObserverKey )

      def isEmpty(  implicit tx: S#Tx ) : Boolean = children.isEmpty    // XXX TODO this is expensive
      def nonEmpty( implicit tx: S#Tx ) : Boolean = children.nonEmpty   // XXX TODO this is expensive

//      private[event] def nodeOption : Option[ Node[ S ]] = None
      private[event] def _targets : Targets[ S ] = this

      private[event] def isInvalid( implicit tx: S#Tx ) : Boolean = !invalidVar.isFresh || (invalidVar.get != 0)

      private[event] def isInvalid( slot: Int  )( implicit tx: S#Tx ) : Boolean =
         !invalidVar.isFresh || ((invalidVar.getOrElse( 0 ) & slot) != 0)

      private[event] def validated( slot: Int )( implicit tx: S#Tx ) {
         if( invalidVar.isFresh ) {
//            invalidVar.transform( _ & ~slot )
            invalidVar.transform( 0 )( _ & ~slot )
         } else {
            invalidVar.set( ~slot )
         }
      }

      private[event] def invalidate( slot: Int )( implicit tx: S#Tx ) {
         if( invalidVar.isFresh ) {
            invalidVar.transform( 0 )( _ | slot )
         } else {
            invalidVar.set( 0xFFFFFFFF )
         }
      }

      private[event] def validated()( implicit tx: S#Tx ) {
         invalidVar.set( 0 )
      }

      private[event] def invalidate()( implicit tx: S#Tx ) {
         invalidVar.set( 0xFFFFFFFF )
      }
   }
}

/**
 * An abstract trait unifying invariant and mutating targets. This object is responsible
 * for keeping track of the dependents of an event source which is defined as the outer
 * object, sharing the same `id` as its targets. As a `Reactor`, it has a method to
 * `propagate` a fired event.
 */
sealed trait Targets[ S <: stm.Sys[ S ]] extends Reactor[ S ] /* extends Writable with Disposable[ S#Tx ] */ {
//   /* private[event] */ def id: S#ID

//   private[event] def children( implicit tx: S#Tx ) : Children[ S ]
   private[event] def children( implicit tx: S#Tx ) : Children[ S ]

   private[lucre] def isPartial : Boolean

   /**
    * Adds a dependant to this node target.
    *
    * @param slot the slot for this node to be pushing to the dependant
    * @param sel  the target selector to which an event at slot `slot` will be pushed
    *
    * @return  `true` if this was the first dependant registered with the given slot, `false` otherwise
    */
   private[event] def add( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) : Boolean

   /**
    * This method should be invoked when the targets are invalid for the given slot. It resets the children
    * for that slot to the single selector, and clears the invalid flag for the slot.
    *
    * @param slot the slot for this node to be pushing to the dependant
    * @param sel  the target selector to which an event at slot `slot` will be pushed
    */
   private[event] def resetAndValidate( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) : Unit

   def isEmpty( implicit tx: S#Tx ) : Boolean
   def nonEmpty( implicit tx: S#Tx ) : Boolean

   /**
    * Removes a dependant from this node target.
    *
    * @param slot the slot for this node which is currently pushing to the dependant
    * @param sel  the target selector which was registered with the slot
    *
    * @return  `true` if this was the last dependant unregistered with the given slot, `false` otherwise
    */
   private[event] def remove( slot: Int, sel: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) : Boolean

   private[event] def observers( implicit tx: S#Tx ): IIdxSeq[ ObserverKey[ S ]]

   private[event] def isInvalid( implicit tx: S#Tx ) : Boolean
   private[event] def validated()( implicit tx: S#Tx ) : Unit
   private[event] def invalidate()( implicit tx: S#Tx ) : Unit

   private[event] def isInvalid( slot: Int  )( implicit tx: S#Tx ) : Boolean
   private[event] def validated( slot: Int )( implicit tx: S#Tx ) : Unit
   private[event] def invalidate( slot: Int )( implicit tx: S#Tx ) : Unit
}

/**
 * An `Event.Node` is most similar to EScala's `EventNode` class. It represents an observable
 * object and can also act as an observer itself. It adds the `Reactor` functionality in the
 * form of a proxy, forwarding to internally stored `Targets`. It also provides a final
 * implementation of the `Writable` and `Disposable` traits, asking sub classes to provide
 * methods `writeData` and `disposeData`. That way it is ensured that the sealed `Reactor` trait
 * is written first as the `Targets` stub, providing a means for partial deserialization during
 * the push phase of event propagation.
 *
 * This trait also implements `equals` and `hashCode` in terms of the `id` inherited from the
 * targets.
 */
/* sealed */ trait Node[ S <: stm.Sys[ S ]] extends /* Reactor[ S ] with */ VirtualNode[ S ] /* with Dispatcher[ S, A ] */ {
   override def toString = "Node" + id

   protected def targets: Targets[ S ]
   protected def writeData( out: DataOutput ) : Unit
   protected def disposeData()( implicit tx: S#Tx ) : Unit

   final protected def validated()( implicit tx: S#Tx ) { targets.validated() }
   final protected def isInvalid( implicit tx: S#Tx ) : Boolean = targets.isInvalid
   final protected def invalidate()( implicit tx: S#Tx ) { targets.invalidate() }

   final private[event] def _targets : Targets[ S ] = targets

   final private[event] def children( implicit tx: S#Tx ) = targets.children

   final def id: S#ID = targets.id

//   private[event] def select( slot: Int ) : NodeSelector[ S, Any ]
   private[event] def select( slot: Int, invariant: Boolean ) : Event[ S, Any, Any ] // NodeSelector[ S, Any ]

   final def write( out: DataOutput ) {
      targets.write( out )
      writeData( out )
   }

   final def dispose()( implicit tx: S#Tx ) {
      disposeData()     // call this first, as it may release events
      targets.dispose()
   }
}

/**
 * The `Reactor` trait encompasses the possible targets (dependents) of an event. It defines
 * the `propagate` method which is used in the push-phase (first phase) of propagation. A `Reactor` is
 * either a persisted event `Node` or a registered `ObserverKey` which is resolved through the transaction
 * as pointing to a live view.
 */
sealed trait Reactor[ S <: stm.Sys[ S ]] extends Mutable[ S#ID, S#Tx ] {
   private[event] def _targets : Targets[ S ]

   override def equals( that: Any ) : Boolean = {
      (if( that.isInstanceOf[ Reactor[ _ ]]) {
         id == that.asInstanceOf[ Reactor[ _ ]].id
      } else super.equals( that ))
   }

   override def hashCode = id.hashCode()
}

object VirtualNode {
   private[event] def read[ S <: EventSys[ S ]]( in: DataInput, fullSize: Int, access: S#Acc )( implicit tx: S#Tx ) : Raw[ S ] = {
      val off        = in.getBufferOffset
      val targets    = Targets.read( in, access )
      val dataSize   = fullSize - (in.getBufferOffset - off)
      val data       = new Array[ Byte ]( dataSize )
      in.read( data )
      new Raw( targets, data, access )
   }

   private[event] final class Raw[ S <: EventSys[ S ]]( private[event] val _targets: Targets[ S ], data: Array[ Byte ], access: S#Acc )
   extends VirtualNode[ S ] {
      def id: S#ID = _targets.id

      def write( out: DataOutput ) {
         _targets.write( out )
         out.write( data )
      }

      private[event] def select( slot: Int, invariant: Boolean ) = Selector( slot, this, invariant )

      private[event] def devirtualize[ Repr ]( reader: Reader[ S, Repr ])( implicit tx: S#Tx ) : Repr with Node[ S ] = {
         val in = new DataInput( data )
         reader.read( in, access, _targets )
      }

      def dispose()( implicit tx: S#Tx ) {
         _targets.dispose()
      }

      override def toString = "VirtualNode.Raw" + _targets.id
   }
}
//private[event] final case class VirtualNode[ S <: Sys[ S ]]( targets: Targets[ S ], data: Array[ Byte ])

sealed trait VirtualNode[ S <: stm.Sys[ S ]] extends Reactor[ S ] {
//   def id: S#ID
   private[event] def select( slot: Int, invariant: Boolean ) : VirtualNodeSelector[ S ]
//   private[event] def devirtualize( reader: Reader[ S, Node[ S ]])( implicit tx: S#Tx ) : Node[ S ]
}

//sealed trait VirtualNode[ S <: Sys[ S ]] extends Reactor[ S ] {
//   private[event] def select( slot: Int, invariant: Boolean ) : VirtualNodeSelector[ S ]
//}
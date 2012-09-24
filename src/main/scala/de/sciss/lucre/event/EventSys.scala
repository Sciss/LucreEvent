package de.sciss.lucre
package event

import stm.{Disposable, Sink}

trait EventSys[ S <: EventSys[ S ]] extends stm.Sys[ S ] {
   type Tx <: EventTxn[ S ]
}
trait EventTxn[ S <: EventSys[ S ]] extends stm.Txn[ S ] {
   def reactionMap: ReactionMap[ S ]
   private[event] def newEventVar[ A ]( id: S#ID )( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : EventVar[ S, A ]
   private[event] def newEventIntVar[ A ]( id: S#ID ) : EventVar[ S, Int ]
   private[event] def readEventVar[ A ]( id: S#ID, in: DataInput )( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : EventVar[ S, A ]
   private[event] def readEventIntVar[ A ]( id: S#ID, in: DataInput ) : EventVar[ S, Int ]
}

trait EventVar[ S <: stm.Sys[ S ], @specialized( Int ) A ] extends Sink[ S#Tx, A ] with Writable with Disposable[ S#Tx ]{
   def get( implicit tx: S#Tx ) : Option[ A ]
   def getOrElse( default: A )( implicit tx: S#Tx ) : A
   def isFresh( implicit tx: S#Tx ) : Boolean
   def transform( default: => A )( f: A => A )( implicit tx: S#Tx ) : Unit
}
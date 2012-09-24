package de.sciss.lucre
package event

trait EventSys[ S <: EventSys[ S ]] extends stm.Sys[ S ] {
   type Tx <: EventTxn[ S ]
}

trait EventTxn[ S <: EventSys[ S ]] extends stm.Txn[ S ] {
   def reactionMap: ReactionMap[ S ]
}
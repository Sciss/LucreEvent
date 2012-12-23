/*
 *  Sys.scala
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

import stm.Disposable

trait Sys[ S <: Sys[ S ]] extends stm.Sys[ S ] {
   type Tx <: Txn[ S ]

   private[lucre] def reactionMap: ReactionMap[ S ]
}
trait Txn[ S <: Sys[ S ]] extends stm.Txn[ S ] {
   private[lucre] def reactionMap: ReactionMap[ S ]
   private[event] def newEventVar[ A ]( id: S#ID )( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ]
   private[event] def newEventIntVar[ A ]( id: S#ID ) : Var[ S, Int ]
   private[event] def readEventVar[ A ]( id: S#ID, in: DataInput )( implicit serializer: stm.Serializer[ S#Tx, S#Acc, A ]) : Var[ S, A ]
   private[event] def readEventIntVar[ A ]( id: S#ID, in: DataInput ) : Var[ S, Int ]
}

trait Var[ S <: stm.Sys[ S ], @specialized( Int ) A ] extends stm.Sink[ S#Tx, A ] with Writable with Disposable[ S#Tx ]{
   def get( implicit tx: S#Tx ) : Option[ A ]
   def getOrElse( default: => A )( implicit tx: S#Tx ) : A
   def isFresh( implicit tx: S#Tx ) : Boolean
   def transform( default: => A )( f: A => A )( implicit tx: S#Tx ) : Unit
}
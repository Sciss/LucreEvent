/*
 *  InMemory.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

import impl.{InMemoryImpl => Impl}

object InMemory {
   def apply() : InMemory = Impl()
}
trait InMemory extends stm.InMemoryLike[ InMemory ] with Sys[ InMemory ] {
   final type Tx     = Txn[ InMemory ]
//   final type IM     = InMemory

//   final def inMemory[ A ]( fun: InMemory#IM#Tx => A )( implicit tx: InMemory#Tx ) : A = fun( tx )

//   private type S = InMemory
//
//   final def im( tx: S#Tx ) : IM#Tx = tx.inMemory
//   final def imVar[ A ]( v: Var[ A ]) : Var[ A ] = v
}

/*
 *  InMemory.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package event

import impl.{InMemoryImpl => Impl}

trait InMemoryLike[S <: InMemoryLike[S]] extends stm.InMemoryLike[S] with Sys[S]

object InMemory {
  def apply(): InMemory = Impl()
}

trait InMemory extends InMemoryLike[InMemory] {
  type Tx = Txn[InMemory]
  type I  = InMemory
}
/*
 *  Singleton.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package event
package impl

import serial.DataOutput

/**
 * A `Singleton` event is one which doesn't carry any state. This is a utility trait
 * which provides no-op implementations for `writeData` and `disposeData`.
 */
trait Singleton[S <: stm.Sys[S] /* , A, Repr */ ] extends InvariantSelector[S] {
  final protected def disposeData()(implicit tx: S#Tx) = ()
  final protected def writeData(out: DataOutput) = ()
}

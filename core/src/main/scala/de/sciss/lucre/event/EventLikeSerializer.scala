/*
 *  EventLikeSerializer.scala
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

import annotation.switch
import serial.{Writable, DataInput, DataOutput}

// XXX TODO: should be in package `impl`

/** A trait to serialize events which can be both constants and immutable nodes.
  * An implementation mixing in this trait just needs to implement methods
  * `readConstant` to return the constant instance, and `read` with the
  * `Event.Targets` argument to return the immutable node instance.
  *
  * The constant event should mix in `Constant` which takes care of writing
  * the appropriate serialization preamble.
  */
trait EventLikeSerializer[S <: Sys[S], Repr <: Writable /* Node[ S ] */ ]
  extends Reader[S, Repr] with serial.Serializer[S#Tx, S#Acc, Repr] {

  final def write(v: Repr, out: DataOutput): Unit = v.write(out)

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr = {
    (in.readByte(): @switch) match {
      case 3 => readConstant(in)
      case 0 =>
        val targets = Targets.readIdentified[S](in, access)
        read(in, access, targets)
      case 1 =>
        val targets = Targets.readIdentifiedPartial[S](in, access)
        read(in, access, targets)
      case cookie => sys.error("Unexpected cookie " + cookie)
    }
  }

  /** Called by the implementation when the cookie for constant value is
    * detected in deserialization.
    *
    * @return  the constant representation of this event like type, which
    *          should mix in trait `Constant`.
    */
  def readConstant(in: DataInput)(implicit tx: S#Tx): Repr
}

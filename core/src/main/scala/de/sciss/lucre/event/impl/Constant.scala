/*
 *  Constant.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
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

/** A constant "event" is one which doesn't actually fire. It thus arguably isn't really an event,
  * but it can be used to implement the constant type of an expression system which can use a unified
  * event approach, where the `Constant` event just acts as a dummy event. `addReactor` and `removeReactor`
  * have no-op implementations. Also `pull` in inherited from `Root`, but will always return `None`
  * as there is no way to fire this event. Implementation must provide a constant value method
  * `constValue` and implement its serialization via `writeData`.
  */
trait Constant {
  final def write(out: DataOutput): Unit = {
    out.writeByte(3)
    writeData(out)
  }

  protected def writeData(out: DataOutput): Unit
}

/*
 *  DummyValidity.scala
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

object DummyValidity extends Validity[Any] {
  def apply  ()(implicit tx: Any) = true  // always valid
  def update ()(implicit tx: Any) = ()    // no-op
  def dispose()(implicit tx: Any) = ()    // no-op

  def write(out: DataOutput)      = ()    // no-op
}
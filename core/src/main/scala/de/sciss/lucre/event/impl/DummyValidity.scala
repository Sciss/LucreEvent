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
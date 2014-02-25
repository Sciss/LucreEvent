package de.sciss.lucre
package expr
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataInput
import evt.Sys
import language.higherKinds
import expr.{Int => _Int}

trait TypeImpl[Repr[~ <: Sys[~]]] extends Type[Repr] {
  private[this] var exts = new Array[Type.Extension[Repr]](0)

  final def registerExtension(ext: Type.Extension[Repr]): Unit = {
    val opLo = ext.opLo
    val opHi = ext.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val idx0  = exts.indexWhere(_.opLo > opHi)
    val idx   = if (idx0 < 0) exts.length else idx0
    if (idx > 0) {
      val pred = exts(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $ext")
    }
    exts = exts.patch(idx, ext :: Nil, 0)
  }

  private[this] def findExt(op: Int): Int = {
    var index = 0
    var low   = 0
    var high  = exts.size - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = exts(index)
      if (ext.opLo <= op) {
        if (ext.opHi >= op) return index
        low = index + 1
      } else {
        high = index - 1
      }
    }
    -1
  }

  final protected def readExtension[S <: evt.Sys[S]](op: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                    (implicit tx: S#Tx): Repr[S] with evt.Node[S] = {
    val idx = findExt(op)
    require(idx >= 0, s"Unknown extension operator $op")
    val ext = exts(idx)
    ext.readExtension(op, in, access, targets)
  }
}

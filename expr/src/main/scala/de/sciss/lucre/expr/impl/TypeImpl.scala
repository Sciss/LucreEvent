package de.sciss.lucre
package expr
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataInput
import evt.Sys
import language.higherKinds
import expr.{Int => _}

trait TypeImpl[Ext >: Null <: Type.Extension] extends Type {
  implicit protected def extTag: reflect.ClassTag[Ext]

  private[this] var exts = new Array[Ext](0)

  final def registerExtension(ext: Ext): Unit = {
    val opLo = ext.opLo
    val opHi = ext.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val idx0  = exts.indexWhere(_.opLo > opHi)
    val idx   = if (idx0 < 0) exts.length else idx0
    if (idx > 0) {
      val pred = exts(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $ext")
    }
    val len   = exts.length
    val exts1 = new Array[Ext](len + 1)
    System.arraycopy(exts, 0, exts1, 0, len)
    exts1(len) = ext
    exts = exts1
  }

  final protected def findExt(op: Int): Ext = {
    var index = 0
    var low   = 0
    var high  = exts.size - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = exts(index)
      if (ext.opLo <= op) {
        if (ext.opHi >= op) return ext
        low = index + 1
      } else {
        high = index - 1
      }
    }
    null
  }
}

trait TypeImpl1[Repr[~ <: Sys[~]]] extends TypeImpl[Type.Extension1[Repr]] with Type1[Repr] {
  final protected val extTag = reflect.classTag[Type.Extension1[Repr]]

  final protected def readExtension[S <: evt.Sys[S]](op: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                    (implicit tx: S#Tx): Repr[S] with evt.Node[S] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[S](op, in, access, targets)
  }
}

trait TypeImpl2[Repr[~ <: Sys[~], _]] extends TypeImpl[Type.Extension2[Repr]] with Type2[Repr] {
  final protected val extTag = reflect.classTag[Type.Extension2[Repr]]

  final protected def readExtension[S <: evt.Sys[S], T1](op: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                        (implicit tx: S#Tx): Repr[S, T1] with evt.Node[S] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[S, T1](op, in, access, targets)
  }
}

trait TypeImpl3[Repr[~ <: Sys[~], _, _]] extends TypeImpl[Type.Extension3[Repr]] with Type3[Repr] {
  final protected val extTag = reflect.classTag[Type.Extension3[Repr]]

  final protected def readExtension[S <: evt.Sys[S], T1, T2](op: Int, in: DataInput, access: S#Acc,
                                                             targets: evt.Targets[S])
                                                            (implicit tx: S#Tx): Repr[S, T1, T2] with evt.Node[S] = {
    val ext = findExt(op)
    if (ext == null) sys.error(s"Unknown extension operator $op")
    ext.readExtension[S, T1, T2](op, in, access, targets)
  }
}

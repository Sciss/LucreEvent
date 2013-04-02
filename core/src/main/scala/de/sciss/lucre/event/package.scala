package de.sciss
package lucre

import collection.immutable.{IndexedSeq => IIdxSeq}
import annotation.elidable
import annotation.elidable.CONFIG
import java.util.{Locale, Date}
import java.text.SimpleDateFormat

package object event {
  type Reaction = () => () => Unit
  private[event] type Children[S <: stm.Sys[S]] = IIdxSeq[(Int, Selector[S])]

  private val emptySeq = IIdxSeq.empty[Nothing]

  //   private[lucre] def NoSources[ S <: Sys[ S ]]  : Sources[ S ]   = emptySeq
  private[lucre] def NoChildren[S <: stm.Sys[S]]: Children[S] = emptySeq

  type Serializer[S <: stm.Sys[S], Repr] = Reader[S, Repr] with serial.Serializer[S#Tx, S#Acc, Repr]

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'evt' ", Locale.US)

  var showLog = false

  @elidable(CONFIG) private[event] def log(what: => String) {
    if (showLog) {
      println(logHeader.format(new Date()) + what)
    }
  }
}
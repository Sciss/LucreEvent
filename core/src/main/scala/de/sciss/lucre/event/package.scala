/*
 *  package.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.collection.immutable.{IndexedSeq => Vec}

package object event {
  type Reaction = () => () => Unit
  // private[event] type Children[S <: stm.Sys[S]] = Vec[(Int, Selector[S])]
  private[event] type Children[S <: stm.Sys[S]] = Vec[(Byte, Selector[S])]

  private val emptySeq = Vec.empty[Nothing]

  //   private[lucre] def NoSources[ S <: Sys[ S ]]  : Sources[ S ]   = emptySeq
  private[lucre] def NoChildren[S <: stm.Sys[S]]: Children[S] = emptySeq

  type Serializer[S <: stm.Sys[S], Repr] = Reader[S, Repr] with serial.Serializer[S#Tx, S#Acc, Repr]

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Lucre' - 'evt' ", Locale.US)

  var showLog = false

  @elidable(CONFIG) private[event] def log(what: => String): Unit =
    if (showLog) println(logHeader.format(new Date()) + what)
}
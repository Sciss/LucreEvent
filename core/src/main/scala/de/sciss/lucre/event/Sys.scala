/*
 *  Sys.scala
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

import stm.Disposable
import serial.{DataInput, Writable}

trait Sys[S <: Sys[S]] extends stm.Sys[S] {
  type Tx <: Txn[S]

  private[lucre] def reactionMap: ReactionMap[S]
}

trait Txn[S <: Sys[S]] extends stm.Txn[S] {
  private[lucre] def reactionMap: ReactionMap[S]
  private[event] def newEventVar[A]    (id: S#ID)(implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Var[S, A]
  private[event] def readEventVar[A]   (id: S#ID, in: DataInput)(implicit serializer: serial.Serializer[S#Tx, S#Acc, A]): Var[S, A]

  private[event] def newEventValidity (id: S#ID): Validity[S#Tx]
  private[event] def readEventValidity(id: S#ID, in: DataInput): Validity[S#Tx]

  // private[event] def newEventIntVar[A] (id: S#ID): Var[S, Int]
  // private[event] def newEventIntVar[A] (id: S#ID): Var[S, Int]
  // private[event] def readEventIntVar[A](id: S#ID, in: DataInput): Var[S, Int]
}

trait Var[S <: Sys[S], A] extends stm.Sink[S#Tx, A] with Writable with Disposable[S#Tx] {
  def get                     (implicit tx: S#Tx): Option[A]
  def getOrElse(default: => A)(implicit tx: S#Tx): A
  // def isFresh                 (implicit tx: S#Tx): Boolean
  def transform(default: => A)(f: A => A)(implicit tx: S#Tx): Unit
}

trait Validity[-Tx] extends Writable with Disposable[Tx] {
  def apply ()(implicit tx: Tx): Boolean
  def update()(implicit tx: Tx): Unit
}
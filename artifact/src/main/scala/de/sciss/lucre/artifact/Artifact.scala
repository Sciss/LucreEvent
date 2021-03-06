/*
 *  Artifact.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.artifact

import java.io.File

import de.sciss.lucre.artifact.impl.{ArtifactImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr
import de.sciss.serial.{DataInput, Serializer}

import scala.annotation.tailrec

object Artifact {
  final val typeID = 0x10008

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = Impl.serializer

  def relativize(parent: File, sub: File): Child = {
    // Note: .getCanonicalFile will resolve symbolic links.
    // In order to support artifacts being symbolic links
    // inside a parent folder, we must not resolve them!

    val can     = sub   .getAbsoluteFile // .getCanonicalFile
    val base    = parent.getAbsoluteFile // .getCanonicalFile

    @tailrec def loop(res: File, left: File): File = {
      if (left == null)
        throw new IllegalArgumentException(s"File $sub is not in a subdirectory of $parent")

      if (left == base) res
      else {
        val last  = left.getName
        val init  = left.getParentFile
        loop(new File(last, res.getPath), init)
      }
    }

    val cf = loop(new File(can.getName), can.getParentFile)
    Child(cf.getPath)
  }

  type Value = File

  // sealed trait Update[S <: Sys[S]]
  // final case class

  object Modifiable {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readMod(in, access)

    def copy[S <: Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Modifiable[S] =
      Impl.copy(from)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer
  }
  trait Modifiable[S <: Sys[S]] extends Artifact[S] {
    def child_=(value: Child)(implicit tx: S#Tx): Unit
  }

  final case class Child(path: String)
}
/** An artifact is a file on an external storage. */
trait Artifact[S <: Sys[S]] extends Expr[S, Artifact.Value] /* Mutable[S#ID, S#Tx] */ {
  import Artifact._
  def location: ArtifactLocation[S]
  def modifiableOption: Option[Modifiable[S]]
  def child(implicit tx: S#Tx): Child
}
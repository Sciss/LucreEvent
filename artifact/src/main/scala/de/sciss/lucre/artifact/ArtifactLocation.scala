/*
 *  ArtifactLocation.scala
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
import de.sciss.lucre.data
import de.sciss.lucre.event.{Publisher, Sys}
import de.sciss.lucre.stm.Mutable
import de.sciss.model
import de.sciss.serial.{DataInput, Serializer}

object ArtifactLocation {
  final val typeID = 0x10003

  def tmp[S <: Sys[S]]()(implicit tx: S#Tx): Modifiable[S] = {
    val dir   = File.createTempFile("artifacts", "tmp")
    dir.delete()
    dir.mkdir()
    dir.deleteOnExit()
    apply(dir)
  }

  def apply[S <: Sys[S]](init: File)(implicit tx: S#Tx): Modifiable[S] =
    Impl.newLocation[S](init)

  object Modifiable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocation.Modifiable[S]] =
      Impl.modLocationSerializer

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactLocation.Modifiable[S] =
      Impl.readModLocation[S](in, access)
  }
  trait Modifiable[S <: Sys[S]] extends ArtifactLocation[S] {
    /** Registers a significant artifact with the system. That is,
      * stores the artifact, which should have a real resource
      * association, as belonging to the system.
      *
      * @param file   the file to turn into a registered artifact
      */
    def add(file: File)(implicit tx: S#Tx): Artifact.Modifiable[S]
    def remove(artifact: Artifact[S])(implicit tx: S#Tx): Unit

    def directory_=(value: File)(implicit tx: S#Tx): Unit
  }

  sealed trait Update[S <: Sys[S]] {
    def location: ArtifactLocation[S]
  }
  final case class Added[S <: Sys[S]](location: ArtifactLocation[S], idx: Int, artifact: Artifact[S])
    extends Update[S]

  final case class Removed[S <: Sys[S]](location: ArtifactLocation[S], idx: Int, artifact: Artifact[S])
    extends Update[S]

  final case class Moved[S <: Sys[S]](location: ArtifactLocation[S], change: model.Change[File]) extends Update[S]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocation[S]] = Impl.locationSerializer

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactLocation[S] =
    Impl.readLocation[S](in, access)
}
/** An artifact location is a directory on an external storage. */
trait ArtifactLocation[S <: Sys[S]] extends Mutable[S#ID, S#Tx] with Publisher[S, ArtifactLocation.Update[S]] {
  def directory(implicit tx: S#Tx): File
  def iterator (implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]]

  def modifiableOption: Option[ArtifactLocation.Modifiable[S]]
}

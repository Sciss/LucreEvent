/*
 *  Map.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package expr

import de.sciss.lucre.{event => evt, data}
import scala.collection.immutable.{IndexedSeq => Vec}
import evt.{EventLike, Sys, Publisher}
import de.sciss.serial.Serializer

object Map {
  object Modifiable {
    def apply[S <: Sys[S], K, V <: Publisher[S, U], U](implicit tx: S#Tx,
                                                       keySerializer  : Serializer[S#Tx, S#Acc, K],
                                                       valueSerializer: Serializer[S#Tx, S#Acc, V]): Modifiable[S, K, V, U] =
      ???
  }

  trait Modifiable[S <: Sys[S], K, V, U] extends Map[S, K, V, U] {
    /** Inserts a new entry into the map.
      *
      * @param  key  the key to insert
      * @param  value the value to store for the given key
      * @return the previous value stored at the key, or `None` if the key was not in the map
      */
    def put(key: K, value: V)(implicit tx: S#Tx): Option[V]

    def +=(kv: (K, V))(implicit tx: S#Tx): this.type

    /** Removes an entry from the map.
      *
      * @param   key  the key to remove
      * @return  the removed value which had been stored at the key, or `None` if the key was not in the map
      */
    def remove(key: K)(implicit tx: S#Tx): Option[V]

    def -=(key: K)(implicit tx: S#Tx): this.type
  }

  final case class Update[S <: Sys[S], K, V, U](map: Map[S, K, V, U], changes: Vec[Change[S, K, V, U]])

  sealed trait Change[S <: Sys[S], K, V, +U] {
    def key  : K
    def value: V
  }

  final case class Added  [S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V, Nothing]
  final case class Removed[S <: Sys[S], K, V](key: K, value: V) extends Change[S, K, V, Nothing]

  final case class Element[S <: Sys[S], K, V, U](key: K, value: V, change: U) extends Change[S, K, V, U]
}
trait Map[S <: Sys[S], K, V, U] extends evt.Node[S] with Publisher[S, Map.Update[S, K, V, U]] {
  def modifiableOption: Option[Map.Modifiable[S, K, V, U]]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  /** Reports the number of entries in the map.
    * This operation may take up to O(n) time, depending on the implementation.
    */
  def size(implicit tx: S#Tx): Int

  def iterator      (implicit tx: S#Tx): data.Iterator[S#Tx, (K, V)]
  def keysIterator  (implicit tx: S#Tx): data.Iterator[S#Tx, K]
  def valuesIterator(implicit tx: S#Tx): data.Iterator[S#Tx, V]

  /** Searches for the map for a given key.
    *
    * @param   key   the key to search for
    * @return  `true` if the key is in the map, `false` otherwise
    */
  def contains(key: K)(implicit tx: S#Tx): Boolean

  /** Queries the value for a given key.
    *
    * @param key  the key to look for
    * @return     the value if it was found at the key, otherwise `None`
    */
  def get(key: K)(implicit tx: S#Tx): Option[V]
}
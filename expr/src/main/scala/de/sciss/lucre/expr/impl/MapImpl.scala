package de.sciss.lucre
package expr
package impl

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys, Event, EventLike}
import data.{Iterator, SkipList}
import expr.Map.Modifiable
import de.sciss.serial.{Serializer, DataOutput}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.breakOut
import scala.annotation.switch

object MapImpl {
  private final class ActiveImpl[S <: Sys[S], K, V <: Publisher[S, U], U](protected val targets: evt.Targets[S],
                                                                          protected val peer: SkipList.Map[S, K, V])
                       (implicit protected val keySerializer: Serializer[S#Tx, S#Acc, K],
                        protected val valueSerializer: evt.Serializer[S, V])
    extends Impl[S, K, V, U] {
    map =>

    protected def registerValue  (value: V)(implicit tx: S#Tx): Unit = value.changed ---> valueChanged
    protected def unregisterValue(value: V)(implicit tx: S#Tx): Unit = value.changed -/-> valueChanged

    protected def reader: evt.Reader[S, Map[S, K, V, U]] = ??? // activeSerializer

    def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case `changed`      .slot => changed
      case CollectionEvent.slot => CollectionEvent
      case `valueChanged` .slot => valueChanged
    }

    protected object valueChanged
      extends evt.impl.EventImpl[S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.InvariantEvent   [S, Map.Update[S, K, V, U], Map[S, K, V, U]] {

      protected def reader: evt.Reader[S, Map[S, K, V, U]] = ??? // activeSerializer

      final val slot = 1

      def node: Map[S, K, V, U] = map

      def connect   ()(implicit tx: S#Tx): Unit = valuesIterator.foreach(registerValue  )
      def disconnect()(implicit tx: S#Tx): Unit = valuesIterator.foreach(unregisterValue)

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Map.Update[S, K, V, U]] = {
        val changes: Vec[Map.Element[S, K, V, U]] = pull.parents(this).flatMap { sel =>
          val evt = sel.devirtualize[U, V](valueSerializer)
          val opt: Option[Map.Element[S, K, V, U]] = ??? // pull(evt).map(Map.Element(evt.node, _))
          opt
        } (breakOut)

        if (changes.isEmpty) None else Some(Map.Update(map, changes))
      }
    }
  }

  private abstract class Impl[S <: Sys[S], K, V, U] extends Modifiable[S, K, V, U] {
    map =>

    // ---- abstract ----

    protected def peer: SkipList.Map[S, K, V]

    protected def registerValue  (value: V)(implicit tx: S#Tx): Unit
    protected def unregisterValue(value: V)(implicit tx: S#Tx): Unit

    implicit protected def keySerializer  : Serializer[S#Tx, S#Acc, K]
    implicit protected def valueSerializer: Serializer[S#Tx, S#Acc, V]

    protected def valueChanged: EventLike[S, Map.Update[S, K, V, U]]

    protected def reader: evt.Reader[S, Map[S, K, V, U]]

    // ---- implemented ----

    final def contains(key: K)(implicit tx: S#Tx): Boolean    = peer.contains(key)
    final def get     (key: K)(implicit tx: S#Tx): Option[V]  = peer.get     (key)

    final def iterator      (implicit tx: S#Tx): Iterator[S#Tx, (K, V)] = peer.iterator
    final def keysIterator  (implicit tx: S#Tx): Iterator[S#Tx,  K    ] = peer.keysIterator
    final def valuesIterator(implicit tx: S#Tx): Iterator[S#Tx,     V ] = peer.valuesIterator

    final def size    (implicit tx: S#Tx): Int      = peer.size
    final def nonEmpty(implicit tx: S#Tx): Boolean  = peer.nonEmpty
    final def isEmpty (implicit tx: S#Tx): Boolean  = peer.isEmpty

    final def modifiableOption: Option[Modifiable[S, K, V, U]] = Some(this)

    final protected def writeData(out: DataOutput): Unit = peer.write(out)

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    override def toString = "Map" + id

    final protected def isConnected(implicit tx: S#Tx) = targets.nonEmpty

    protected object CollectionEvent
      extends evt.impl.TriggerImpl[S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.impl.EventImpl     [S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.InvariantEvent     [S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.impl.Root          [S, Map.Update[S, K, V, U]] {
      protected def reader = map.reader

      final val slot = 0

      def node: Map[S, K, V, U] = map
    }

    object changed
      extends evt.impl.EventImpl[S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.InvariantEvent   [S, Map.Update[S, K, V, U], Map[S, K, V, U]] {

      protected def reader: evt.Reader[S, Map[S, K, V, U]] = map.reader

      final val slot = 2

      def node: Map[S, K, V, U] = map

      def connect   ()(implicit tx: S#Tx): Unit = {
        CollectionEvent ---> this
        valueChanged    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        CollectionEvent -/-> this
        valueChanged    -/-> this
      }

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Map.Update[S, K, V, U]] = {
        val collOpt = if (pull.contains(CollectionEvent)) pull(CollectionEvent) else None
        val elemOpt = if (pull.contains(valueChanged)) pull(valueChanged) else None

        (collOpt, elemOpt) match {
          case (Some(_), None) => collOpt
          case (None, Some(_)) => elemOpt
          case (Some(Map.Update(_, coll)), Some(Map.Update(_, elem))) =>
            Some(Map.Update(map, coll ++ elem))
          case _ => None
        }
      }
    }

    //    private def fireAdded(idx: Int, elem: Elem)(implicit tx: S#Tx): Unit =
    //      CollectionEvent(List.Update(list, Vec(List.Added(idx, elem))))
    //
    //    private def fireRemoved(idx: Int, elem: Elem)(implicit tx: S#Tx): Unit =
    //      CollectionEvent(List.Update(list, Vec(List.Removed(idx, elem))))

    final def +=(kv: (K, V))(implicit tx: S#Tx): this.type = {
      put(kv._1, kv._2)
      this
    }

    final def -=(key: K)(implicit tx: S#Tx): this.type = {
      remove(key)
      this
    }

    final def put(key: K, value: V)(implicit tx: S#Tx): Option[V] = {
      val old = peer.add(key -> value)
      if (isConnected) {
        old.foreach(unregisterValue)
        registerValue(value)
        ??? // fireAdded(key, value)
      }
      old
    }

    final def remove(key: K)(implicit tx: S#Tx): Option[V] = {
      val valueOpt = peer.remove(key)
      if (isConnected) valueOpt.foreach { value =>
        ??? // fireRemoved(idx, elem)
      }
      valueOpt
    }
  }
}

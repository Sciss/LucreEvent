package de.sciss.lucre
package expr
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event._
import data.{Iterator, SkipList}
import expr.Map.Modifiable
import de.sciss.serial.{DataInput, Serializer, DataOutput}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.breakOut
import scala.annotation.switch

/*
  XXX TODO: Should have a real hash map and not require an `Ordering[K]`
 */
object MapImpl {
  def activeSerializer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                valueSerializer: evt.Serializer[S, V])
  : Serializer[S#Tx, S#Acc, Map[S, K, V, U]] with evt.Reader[S, Map[S, K, V, U]] = ??? // new ActiveSer[S, K, V, U]

  private class ActiveSer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                   valueSerializer: evt.Serializer[S, V],
                                                                   keyOrdering: Ordering[K])
    extends NodeSerializer[S, Map[S, K, V, U]] with evt.Reader[S, Map[S, K, V, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Map[S, K, V, U] =
      MapImpl.activeRead(in, access, targets)
  }

  private def activeRead[S <: Sys[S], K, V <: Publisher[S, U], U](in: DataInput, access: S#Acc, _targets: evt.Targets[S])
                                                                 (implicit tx: S#Tx,
                                                                  _keySer: Serializer[S#Tx, S#Acc, K],
                                                                  _valueSer: evt.Serializer[S, V],
                                                                  keyOrdering: data.Ordering[S#Tx, K])
  : Impl[S, K, V, Pair[S, K, V, U], U] =
    new ActiveImpl[S, K, V, U] {
      val targets         = _targets
      val keySerializer   = _keySer
      val valueSerializer = _valueSer
      val peer            = SkipList.Map.read[S, K, Pair[S, K, V, U]](in, access)(tx, keyOrdering, keySerializer, elemSerializer)
    }

  private final class Pair[S <: Sys[S], K, V <: Publisher[S, U], U](map: ActiveImpl[S, K, V, U],
                                                                    protected val targets: evt.Targets[S],
                                                                    val key: K, val value: V)
    extends evt.impl.StandaloneLike[S, U, Pair[S, K, V, U]] {

    import map.{keySerializer, valueSerializer}

    type Elem = Pair[S, K, V, U]

    def changed: Event[S, U, Elem] = this

    def connect   ()(implicit tx: S#Tx): Unit = value.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = value.changed -/-> this

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[U] = value.changed.pullUpdate(pull)

    protected def writeData(out: DataOutput): Unit = {
      keySerializer  .write(key  , out)
      valueSerializer.write(value, out)
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def reader: evt.Reader[S, Elem] = map.elemSerializer
  }

  private abstract class ActiveImpl[S <: Sys[S], K, V <: Publisher[S, U], U]
    extends Impl[S, K, V, Pair[S, K, V, U], U] {
    map =>

    private type Elem = Pair[S, K, V, U]

    implicit def valueSerializer: evt.Serializer[S, V]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit = elem.changed ---> elementChanged
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit = elem.changed -/-> elementChanged

    protected def reader: evt.Reader[S, Map[S, K, V, U]] = activeSerializer[S, K, V, U]

    object elemSerializer extends Serializer[S#Tx, S#Acc, Elem] with evt.Reader[S, Elem] {
      def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Elem = {
        val key   = keySerializer  .read(in, access)
        val value = valueSerializer.read(in, access)
        new Pair(map, targets, key, value)
      }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem = {
        val targets = evt.Targets.read(in, access)
        read(in, access, targets)
      }

      def write(elem: Elem, out: DataOutput): Unit = elem.write(out)
    }

    protected def mkElem(key: K, value: V)(implicit tx: S#Tx): Elem = {
      val targets = evt.Targets[S]
      new Pair(map, targets, key, value)
    }

    protected def elemValue(elem: Elem): V = elem.value

    def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case changed        .slot => changed
      case CollectionEvent.slot => CollectionEvent
      case elementChanged .slot => elementChanged
    }

    protected object elementChanged
      extends evt.impl.EventImpl[S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.InvariantEvent   [S, Map.Update[S, K, V, U], Map[S, K, V, U]] {

      protected def reader: evt.Reader[S, Map[S, K, V, U]] = activeSerializer[S, K, V, U]

      final val slot = 1

      def node: Map[S, K, V, U] = map

      def connect   ()(implicit tx: S#Tx): Unit = foreach(registerElement  )
      def disconnect()(implicit tx: S#Tx): Unit = foreach(unregisterElement)

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Map.Update[S, K, V, U]] = {
        val changes: Vec[Map.Element[S, K, V, U]] = pull.parents(this).flatMap { sel =>
          val evt   = sel.devirtualize[U, Elem](elemSerializer)
          val elem  = evt.node
          val opt: Option[Map.Element[S, K, V, U]] = pull(evt).map(Map.Element(elem.key, elem.value, _))
          opt
        } (breakOut)

        if (changes.isEmpty) None else Some(Map.Update(map, changes))
      }
    }
  }

  private abstract class Impl[S <: Sys[S], K, V, Elem, U] extends Modifiable[S, K, V, U] {
    map =>

    // ---- abstract ----

    protected def peer: SkipList.Map[S, K, Elem]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit

    implicit def keySerializer  : Serializer[S#Tx, S#Acc, K]
    implicit def valueSerializer: Serializer[S#Tx, S#Acc, V]

    protected def elementChanged: EventLike[S, Map.Update[S, K, V, U]]

    protected def reader: evt.Reader[S, Map[S, K, V, U]]

    protected def mkElem(key: K, value: V)(implicit tx: S#Tx): Elem
    protected def elemValue(elem: Elem): V

    // ---- implemented ----

    final def contains(key: K)(implicit tx: S#Tx): Boolean    = peer.contains(key)
    final def get     (key: K)(implicit tx: S#Tx): Option[V]  = peer.get     (key).map(elemValue)

    final def iterator      (implicit tx: S#Tx): Iterator[S#Tx, (K, V)] = peer.iterator.map {
      case (key, elem) => key -> elemValue(elem)
    }
    final def keysIterator  (implicit tx: S#Tx): Iterator[S#Tx,  K    ] = peer.keysIterator
    final def valuesIterator(implicit tx: S#Tx): Iterator[S#Tx,     V ] = peer.valuesIterator.map(elemValue)

    final def size    (implicit tx: S#Tx): Int      = peer.size
    final def nonEmpty(implicit tx: S#Tx): Boolean  = peer.nonEmpty
    final def isEmpty (implicit tx: S#Tx): Boolean  = peer.isEmpty

    final def modifiableOption: Option[Modifiable[S, K, V, U]] = Some(this)

    final protected def writeData(out: DataOutput): Unit = peer.write(out)

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    override def toString = "Map" + id

    final protected def isConnected(implicit tx: S#Tx) = targets.nonEmpty

    final protected def foreach(fun: Elem => Unit)(implicit tx: S#Tx): Unit =
      peer.valuesIterator.foreach(fun)

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
        elementChanged  ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        CollectionEvent -/-> this
        elementChanged  -/-> this
      }

      private[lucre] def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Map.Update[S, K, V, U]] = {
        val collOpt = if (pull.contains(CollectionEvent)) pull(CollectionEvent) else None
        val elemOpt = if (pull.contains(elementChanged   )) pull(elementChanged   ) else None

        (collOpt, elemOpt) match {
          case (Some(_), None) => collOpt
          case (None, Some(_)) => elemOpt
          case (Some(Map.Update(_  , coll)), Some(Map.Update(_, elem))) =>
            Some    (Map.Update(map, coll ++ elem))
          case _ => None
        }
      }
    }

    private def fireAdded(key: K, value: V)(implicit tx: S#Tx): Unit =
      CollectionEvent(Map.Update(map, Vec(Map.Added(key, value))))

    private def fireRemoved(key: K, value: V)(implicit tx: S#Tx): Unit =
      CollectionEvent(Map.Update(map, Vec(Map.Removed(key, value))))

    final def +=(kv: (K, V))(implicit tx: S#Tx): this.type = {
      put(kv._1, kv._2)
      this
    }

    final def -=(key: K)(implicit tx: S#Tx): this.type = {
      remove(key)
      this
    }

    final def put(key: K, value: V)(implicit tx: S#Tx): Option[V] = {
      val elem  = mkElem(key, value)
      val old = peer.add(key -> elem)
      if (isConnected) {
        old.foreach(unregisterElement)
        registerElement(elem)
        fireAdded(key, value)
      }
      old.map(elemValue)
    }

    final def remove(key: K)(implicit tx: S#Tx): Option[V] = {
      val valueOpt = peer.remove(key).map(elemValue)
      if (isConnected) valueOpt.foreach { value =>
        fireRemoved(key, value)
      }
      valueOpt
    }
  }
}

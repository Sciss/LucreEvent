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
import expr.{Boolean => _Boolean, Int => _Int, String => _String}

object MapImpl {
  def apply[S <: Sys[S], K, V <: Publisher[S, U], U](implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, K],
                                                     valueSerializer: evt.Serializer[S, V]): Modifiable[S, K, V, U] = {
    val targets = evt.Targets[S]
    new ActiveImpl[S, K, V, U](targets) {
      val peer = SkipList.Map.empty[S, K, Vec[ElemImpl[S, K, V, U]]](tx, keyOrdering, keySerializer,
        Serializer.indexedSeq(elemSerializer))
    }
  }

  def activeSerializer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                valueSerializer: evt.Serializer[S, V])
  : Serializer[S#Tx, S#Acc, Map[S, K, V, U]] with evt.Reader[S, Map[S, K, V, U]] = new ActiveSer[S, K, V, U]

  def activeModifiableSerializer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                valueSerializer: evt.Serializer[S, V])
  : Serializer[S#Tx, S#Acc, Modifiable[S, K, V, U]] with evt.Reader[S, Modifiable[S, K, V, U]] =
    new ActiveModSer[S, K, V, U]

  private class ActiveSer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                   valueSerializer: evt.Serializer[S, V])
    extends NodeSerializer[S, Map[S, K, V, U]] with evt.Reader[S, Map[S, K, V, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Map[S, K, V, U] =
      MapImpl.activeRead(in, access, targets)
  }

  private class ActiveModSer[S <: Sys[S], K, V <: Publisher[S, U], U](implicit keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                   valueSerializer: evt.Serializer[S, V])
    extends NodeSerializer[S, Modifiable[S, K, V, U]] with evt.Reader[S, Modifiable[S, K, V, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Modifiable[S, K, V, U] =
      MapImpl.activeRead(in, access, targets)
  }

  def activeRead[S <: Sys[S], K, V <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                                                         (implicit tx: S#Tx,
                                                          keySerializer: Serializer[S#Tx, S#Acc, K],
                                                          valueSerializer: evt.Serializer[S, V]): Map[S, K, V, U] =
    activeModifiableRead(in, access)  // currently the same

  def activeModifiableRead[S <: Sys[S], K, V <: Publisher[S, U], U](in: DataInput, access: S#Acc)
                                                         (implicit tx: S#Tx,
                                                          keySerializer: Serializer[S#Tx, S#Acc, K],
                                                          valueSerializer: evt.Serializer[S, V]): Modifiable[S, K, V, U] = {
    val targets = evt.Targets.read(in, access)
    activeRead(in, access, targets)
  }


  private def activeRead[S <: Sys[S], K, V <: Publisher[S, U], U](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                                 (implicit tx: S#Tx,
                                                                  keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                  valueSerializer: evt.Serializer[S, V])
  : Impl[S, K, V, ElemImpl[S, K, V, U], U] =
    new ActiveImpl[S, K, V, U](targets) {
      val peer = SkipList.Map.read[S, K, Vec[ElemImpl[S, K, V, U]]](in, access)(tx, keyOrdering, keySerializer,
        Serializer.indexedSeq(elemSerializer))
    }

  private final class ElemImpl[S <: Sys[S], K, V <: Publisher[S, U], U](map: ActiveImpl[S, K, V, U],
                                                                    protected val targets: evt.Targets[S],
                                                                    val key: K, val value: V)
    extends evt.impl.StandaloneLike[S, U, ElemImpl[S, K, V, U]] {

    import map.{keySerializer, valueSerializer}

    type Elem = ElemImpl[S, K, V, U]

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

  private abstract class ActiveImpl[S <: Sys[S], K, V <: Publisher[S, U], U](protected val targets: evt.Targets[S])
                                                                            (implicit val keySerializer: Serializer[S#Tx, S#Acc, K],
                                                                             val valueSerializer: evt.Serializer[S, V])
    extends Impl[S, K, V, ElemImpl[S, K, V, U], U] {
    map =>

    private type Elem = ElemImpl[S, K, V, U]

    final protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit = elem.changed ---> elementChanged
    final protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit = elem.changed -/-> elementChanged

    final protected def reader: evt.Reader[S, Map[S, K, V, U]] = activeSerializer[S, K, V, U]

    implicit object keyOrdering extends data.Ordering[S#Tx, K] {
      def compare(a: K, b: K)(implicit tx: S#Tx): Int = {
        val ah = a.##
        val bh = b.##
        if (ah < bh) -1 else if (ah > bh) 1 else 0
      }
    }

    object elemSerializer extends Serializer[S#Tx, S#Acc, Elem] with evt.Reader[S, Elem] {
      def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Elem = {
        val key   = keySerializer  .read(in, access)
        val value = valueSerializer.read(in, access)
        new ElemImpl(map, targets, key, value)
      }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem = {
        val targets = evt.Targets.read(in, access)
        read(in, access, targets)
      }

      def write(elem: Elem, out: DataOutput): Unit = elem.write(out)
    }

    final protected def mkElem(key: K, value: V)(implicit tx: S#Tx): Elem = {
      val targets = evt.Targets[S]
      new ElemImpl(map, targets, key, value)
    }

    final protected def elemKey  (elem: Elem): K = elem.key
    final protected def elemValue(elem: Elem): V = elem.value

    final def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
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

    protected def peer: SkipList.Map[S, K, Vec[Elem]]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit

    implicit def keySerializer  : Serializer[S#Tx, S#Acc, K]
    implicit def valueSerializer: Serializer[S#Tx, S#Acc, V]

    protected def elementChanged: EventLike[S, Map.Update[S, K, V, U]]

    protected def reader: evt.Reader[S, Map[S, K, V, U]]

    protected def mkElem(key: K, value: V)(implicit tx: S#Tx): Elem
    protected def elemKey  (elem: Elem): K
    protected def elemValue(elem: Elem): V

    // ---- implemented ----

    final def contains(key: K)(implicit tx: S#Tx): Boolean    = peer.get(key).exists(vec => vec.exists(elemKey(_) == key))
    final def get     (key: K)(implicit tx: S#Tx): Option[V]  = peer.get(key).flatMap { vec =>
      vec.find(elemKey(_) == key).map(elemValue)
    }

    final def iterator      (implicit tx: S#Tx): Iterator[S#Tx, (K, V)] = peer.iterator.flatMap {
      case (key, vec) => vec.map(key -> elemValue(_))
    }
    final def keysIterator  (implicit tx: S#Tx): Iterator[S#Tx,  K    ] = peer.valuesIterator.flatMap(_.map(elemKey  ))
    final def valuesIterator(implicit tx: S#Tx): Iterator[S#Tx,     V ] = peer.valuesIterator.flatMap(_.map(elemValue))

    final def size(implicit tx: S#Tx): Int = {
      // XXX TODO: poco ugly...
      var res = 0
      peer.valuesIterator.foreach(res += _.size)
      res
    }
    final def nonEmpty(implicit tx: S#Tx): Boolean  = peer.nonEmpty
    final def isEmpty (implicit tx: S#Tx): Boolean  = peer.isEmpty

    final def modifiableOption: Option[Modifiable[S, K, V, U]] = Some(this)

    final protected def writeData(out: DataOutput): Unit = peer.write(out)

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    override def toString = "Map" + id

    final protected def isConnected(implicit tx: S#Tx) = targets.nonEmpty

    final protected def foreach(fun: Elem => Unit)(implicit tx: S#Tx): Unit =
      peer.valuesIterator.foreach(_.foreach(fun))

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
        val elemOpt = if (pull.contains(elementChanged )) pull(elementChanged ) else None

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
      val elem      = mkElem(key, value)
      val oldVec    = peer.get(key).getOrElse(Vec.empty)
      val idx       = oldVec.indexWhere(elemKey(_) == key)
      val found     = idx >= 0
      val newVec    = if (found) oldVec.updated(idx, elem) else oldVec :+ elem
      peer.add(key -> newVec)
      if (isConnected) {
        if (found) {
          val oldElem = oldVec(idx)
          unregisterElement(oldElem)
          fireRemoved(key, elemValue(oldElem))
        }
        registerElement(elem)
        fireAdded(key, value)
      }
      if (found) Some(elemValue(oldVec(idx))) else None
    }

    final def remove(key: K)(implicit tx: S#Tx): Option[V] = {
      val oldVec  = peer.get(key).getOrElse(Vec.empty)
      val idx     = oldVec.indexWhere(elemKey(_) == key)
      if (idx < 0) return None

      val elem    = oldVec(idx)
      val value   = elemValue(elem)
      val newVec  = oldVec.patch(idx, Nil, 1)
      if (newVec.isEmpty) {
        peer.remove(key)
      } else {
        peer.add(key -> newVec)
      }

      if (isConnected) {
        unregisterElement(elem)
        fireRemoved(key, value)
      }
      Some(value)
    }
  }
}
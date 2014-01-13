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
  private final class Pair[S <: Sys[S], K, V <: Publisher[S, U], U](protected val targets : evt.Targets[S],
                                                                    val key: K, val value: V)
    { // extends evt.impl.StandaloneLike[S, BiGroup.Update[S, Elem, U], TimedElem[S, Elem]] with TimedElem[S, Elem] {
//
//    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
//      var res     = Vector.empty[BiGroup.Change[S, Elem, U]]
//      val spanEvt = span.changed
//      if (pull.contains(spanEvt)) {
//        pull(spanEvt).foreach { ch =>
//          log(s"$this.pullUpdate -> ElementMoved")
//          res :+= BiGroup.ElementMoved(this, ch)
//        }
//      }
//      val valueEvt = eventView(value)
//      if (pull.contains(valueEvt)) {
//        pull(valueEvt).foreach {
//          ch => res :+= BiGroup.ElementMutated(this, ch)
//        }
//      }
//
//      if (res.nonEmpty) Some(BiGroup.Update(group, res)) else None
//    }
//
//    protected def writeData(out: DataOutput): Unit = {
//      span.write(out)
//      elemSerializer.write(value, out)
//    }
//
//    protected def disposeData()(implicit tx: S#Tx) = ()
//
//    def connect()(implicit tx: S#Tx): Unit = {
//      log(s"$this.connect()")
//      span.changed     ---> this
//      eventView(value) ---> this
//    }
//
//    def disconnect()(implicit tx: S#Tx): Unit = {
//      log(s"$this.disconnect()")
//      span.changed     -/-> this
//      eventView(value) -/-> this
//    }
//
//    protected def reader: evt.Reader[S, TimedElemImpl[S, Elem, U]] = group.TimedSer
  }

  private final class ActiveImpl[S <: Sys[S], K, V <: Publisher[S, U], U](
    protected val targets: evt.Targets[S],
    protected val peer: SkipList.Map[S, K, Pair[S, K, V, U]])
   (implicit protected val keySerializer: Serializer[S#Tx, S#Acc, K],
             protected val valueSerializer: evt.Serializer[S, V])
    extends Impl[S, K, V, Pair[S, K, V, U], U] {
    map =>

    private type Elem = Pair[S, K, V, U]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit = ??? // elem.changed ---> elementChanged
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit = ??? // elem.changed -/-> elementChanged

    protected def reader: evt.Reader[S, Map[S, K, V, U]] = ??? // activeSerializer

    protected def mkElem(key: K, value: V)(implicit tx: S#Tx): Elem = {
      ???
    }

    protected def elemValue(elem: Elem): V = elem.value

    def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case `changed`        .slot => changed
      case CollectionEvent  .slot => CollectionEvent
      case `elementChanged` .slot => elementChanged
    }

    protected object elementChanged
      extends evt.impl.EventImpl[S, Map.Update[S, K, V, U], Map[S, K, V, U]]
      with evt.InvariantEvent   [S, Map.Update[S, K, V, U], Map[S, K, V, U]] {

      protected def reader: evt.Reader[S, Map[S, K, V, U]] = ??? // activeSerializer

      final val slot = 1

      def node: Map[S, K, V, U] = map

      def connect   ()(implicit tx: S#Tx): Unit = foreach(registerElement  )
      def disconnect()(implicit tx: S#Tx): Unit = foreach(unregisterElement)

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

  private abstract class Impl[S <: Sys[S], K, V, Elem, U] extends Modifiable[S, K, V, U] {
    map =>

    // ---- abstract ----

    protected def peer: SkipList.Map[S, K, Elem]

    protected def registerElement  (elem: Elem)(implicit tx: S#Tx): Unit
    protected def unregisterElement(elem: Elem)(implicit tx: S#Tx): Unit

    implicit protected def keySerializer  : Serializer[S#Tx, S#Acc, K]
    implicit protected def valueSerializer: Serializer[S#Tx, S#Acc, V]

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
        elementChanged    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        CollectionEvent -/-> this
        elementChanged    -/-> this
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
      val elem  = mkElem(key, value)
      val old = peer.add(key -> elem)
      if (isConnected) {
        old.foreach(unregisterElement)
        registerElement(elem)
        ??? // fireAdded(key, value)
      }
      old.map(elemValue)
    }

    final def remove(key: K)(implicit tx: S#Tx): Option[V] = {
      val valueOpt = peer.remove(key)
      if (isConnected) valueOpt.foreach { value =>
        ??? // fireRemoved(idx, elem)
      }
      valueOpt.map(elemValue)
    }
  }
}

///*
// *  Compound.scala
// *  (LucreEvent)
// *
// *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre
//package event
//
//import collection.breakOut
//import collection.immutable.{IndexedSeq => IIdxSeq}
//import language.implicitConversions
//import reflect.ClassTag
//
//object Compound {
//   private def opNotSupported = sys.error( "Operation not supported" )
//
//   final protected class EventOps1[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], B ](
//      d: Repr, e: Event[ S, B, Any ]) {
//      def map[ A1 /* <: D#Update */ ]( fun: B => A1 )( implicit m: ClassTag[ A1 ]) : Event[ S, A1, Repr ] =
//         new Map[ S, D, Repr, B, A1 ]( d, e, _ => fun )
//
//      def mapTx[ A1 /* <: D#Update */]( fun: S#Tx => B => A1 )( implicit m: ClassTag[ A1 ]) : Event[ S, A1, Repr ] =
//         new Map[ S, D, Repr, B, A1 ]( d, e, fun )
//
//      def mapAndMutate[ A1 /* <: D#Update */]( fun: S#Tx => B => A1 )( implicit m: ClassTag[ A1 ]) : MutatingEvent[ S, A1, Repr ] =
//         new MutatingMap[ S, D, Repr, B, A1 ]( d, e, fun )
//   }
//
//   final protected class EventOps2[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], B /* <: D#Update */ ](
//      d: Repr /* Compound[ S, Repr, D ] */, e: Event[ S, B, Repr ]) {
//      def |[ Up >: B /* <: D#Update */, C <: Up ]( that: Event[ S, C, Repr ]) : Or[ S, D, Repr, Up ] =
//         new Or[ S, D, Repr, Up ]( d, IIdxSeq[ Event[ S, Up, Repr ]]( e, that ))
//   }
//
//   final class Or[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], B /* <: D#Update */] private[Compound](
//      val node: Repr, elems: IIdxSeq[ Event[ S, B, Repr ]])
//   extends Event[ S, B, Repr ] with InvariantSelector[ S ] {
//
//// XXX
////protected def cookie = opNotSupported
////private[event] def pushUpdate( parent: VirtualNodeSelector[ S ], push: Push[ S ]) { opNotSupported }
//private[event] def slot = opNotSupported
//
//
//      def react[ B1 >: B ]( fun: B1 => Unit )( implicit tx: S#Tx ) : Observer[ S, B1, Repr ] = reactTx( _ => fun )
//
//      def reactTx[ B1 >: B ]( fun: S#Tx => B1 => Unit )( implicit tx: S#Tx ) : Observer[ S, B1, Repr ] = {
//         val obs = Observer( node.decl.serializer, fun )
//         elems.foreach( obs.add )
//         obs
//      }
//
//     private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[B] = {
//       elems.find(ev => ev.isSource(pull)).flatMap(pull(_))
//     }
//
//     /* private[lucre] */ def isSource( pull: Pull[ S ]) : Boolean = opNotSupported
//
////      private[lucre] def select() = opNotSupported
//
//      private[lucre] def connect()( implicit tx: S#Tx ) {}
//      private[lucre] def reconnect()( implicit tx: S#Tx ) {
////         elems.foreach( _.reconnect() )
//      }
//      private[lucre] def disconnect()( implicit tx: S#Tx ) {}
//
//      /* private[lucre] */ def --->( r: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) {
//         elems.foreach( _ ---> r )
//      }
//      /* private[lucre] */ def -/->( r: /* MMM Expanded */ Selector[ S ])( implicit tx: S#Tx ) {
//         elems.foreach( _ -/-> r )
//      }
//
//      def |[ Up >: B /* <: D#Update */, C <: Up ]( that: Event[ S, C, Repr ]) : Or[ S, D, Repr, Up ] =
//         new Or[ S, D, Repr, Up ]( node, IIdxSeq[ Event[ S, Up, Repr ]]( elems: _* ) :+ that )
//
//      override def toString = elems.mkString( " | " )
//   }
//
//   final protected class CollectionOps[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], Elem <: Node[ S ], B ](
//      d: Repr, elem: Elem => EventLike[ S, B, Elem ])( implicit elemReader: Reader[ S, Elem ]) {
//
//      def map[ A1 /* <: D#Update */]( fun: IIdxSeq[ B ] => A1 )( implicit m: ClassTag[ A1 ]) : CollectionEvent[ S, D, Repr, Elem, B, A1 ] =
//         new CollectionEvent[ S, D, Repr, Elem, B, A1 ]( d, elem, fun )
//   }
//
//   sealed trait EventImpl[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], A1 /* <: D#Update */]
//   extends impl.EventImpl[ S, /* D#Update, */ A1, Repr ] {
//      def node: Repr // Compound[ S, Repr, D ]
//      protected def prefix : String
//      implicit protected def m: ClassTag[ A1 ]
//
//      final protected def reader: Reader[ S, Repr ] = node.decl.serializer // [ S ]
//
//      final private[event] def slot = node.decl.eventID[ A1 ]
//
//      override def toString = prefix + "[" + {
//         val mn = m.toString()
//         val i  = math.max( mn.lastIndexOf( '$' ), mn.lastIndexOf( '.' )) + 1
//         mn.substring( i )
//      } + "]"
//   }
//
//   final class CollectionEvent[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], Elem <: Node[ S ], B, A1 /* <: D#Update */] private[Compound](
//      val node: Repr, elemEvt: Elem => EventLike[ S, B, Elem ], fun: IIdxSeq[ B ] => A1 )
//   ( implicit elemReader: Reader[ S, Elem ], protected val m: ClassTag[ A1 ])
//   extends EventImpl[ S, D, Repr, A1 ] with InvariantEvent[ S, A1, Repr ] {
//
//      private[lucre] def connect()( implicit tx: S#Tx ) {}
//      private[lucre] def disconnect()( implicit tx: S#Tx ) {}
//
//      def +=( elem: Elem )( implicit tx: S#Tx ) {
//         elemEvt( elem ) ---> this
////         tx._writeUgly( reactor.id, elem.id, elem )
//      }
//
//      def -=( elem: Elem )( implicit tx: S#Tx ) {
//         elemEvt( elem ) -/-> this
//      }
//
//      protected def prefix = node.toString + ".event"
//
//     private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A1] = {
//       val elems: IIdxSeq[B] = pull.parents(this /* select() */).flatMap(sel => {
//         val evt = sel.devirtualize[B, Any](elemReader)
//         pull(evt)
//       })(breakOut)
//
//       if (elems.isEmpty) None else Some(fun(elems))
//     }
//   }
//
//   private sealed trait MapLike[ S <: Sys[ S ], D <: Decl[ S, Repr ], Repr <: Compound[ S, D, Repr ], B, A1 /* <: D#Update */]
//   extends EventImpl[ S, D, Repr, A1 ] {
//      protected def e: Event[ S, B, Any ]
//
//      private[lucre] def connect()(    implicit tx: S#Tx ) { e ---> this }
//      private[lucre] def disconnect()( implicit tx: S#Tx ) { e -/-> this }
//   }
//
//  private final class Map[S <: Sys[S], D <: Decl[S, Repr], Repr <: Compound[S, D, Repr], B, A1](
//    val node: Repr, protected val e: Event[S, B, Any], fun: S#Tx => B => A1)(implicit protected val m: ClassTag[A1])
//    extends MapLike[S, D, Repr, B, A1] with InvariantEvent[S, A1, Repr] {
//
//    private[lucre] def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A1] = {
//      pull(e).map(fun(tx)(_))
//    }
//
//    protected def prefix = e.toString + ".map"
//  }
//
//  private final class MutatingMap[S <: Sys[S], D <: Decl[S, Repr], Repr <: Compound[S, D, Repr], B, A1](
//    val node: Repr, protected val e: Event[S, B, Any], fun: S#Tx => B => A1)(implicit protected val m: ClassTag[A1])
//    extends MapLike[S, D, Repr, B, A1] with MutatingEvent[S, A1, Repr] {
//
//    protected def processUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[A1] = {
//      pull(e).map(fun(tx)(_))
//    }
//
//    protected def prefix = e.toString + ".mapAndMutate"
//  }
//
//  private final class Trigger[S <: Sys[S], D <: Decl[S, Repr], Repr <: Compound[S, D, Repr], A1](
//    val node: Repr)(implicit protected val m: ClassTag[A1])
//    extends EventImpl[S, D, Repr, A1] with impl.TriggerImpl[S, A1, Repr] with impl.Root[S, A1]
//    with InvariantEvent[S, A1, Repr] {
//
//    protected def prefix = node.toString + ".event"
//  }
//
//}
////trait Compound[ S <: Sys[ S ], Repr <: Compound[ S, Repr, D ], D <: Decl[ S, Repr ]] extends Node[ S ]
//trait Compound[S <: Sys[S], D <: Decl[S, Repr], Repr <: Compound[S, D, Repr]] extends Node[S] {
//  me: Repr =>
//
//  import de.sciss.lucre.{event => evt}
//
//  protected type Ev[A /* <: D#Update */ ] = Event[S, A, Repr]
//
//  protected def decl: D // Decl[ Repr ]
//
//  implicit protected def eventOps1[B](e: Event[S, B, Any]): Compound.EventOps1[S, D, Repr, B] =
//    new Compound.EventOps1(this, e)
//
//  implicit protected def eventOps2[B /* <: D#Update */ ](e: Event[S, B, Repr]): Compound.EventOps2[S, D, Repr, B] =
//    new Compound.EventOps2(this, e)
//
//  protected def event[A1 /* <: D#Update */ ](implicit m: ClassTag[A1]): evt.Trigger[S, A1, Repr] =
//    new Compound.Trigger[S, D, Repr, A1](this)
//
//  protected def collection[Elem <: Node[S], B](fun: Elem => EventLike[S, B, Elem])
//                                              (implicit elemReader: Reader[S, Elem]): Compound.CollectionOps[S, D, Repr, Elem, B] =
//    new Compound.CollectionOps[S, D, Repr, Elem, B](this, fun)
//
//  final private[lucre] def select(slot: Int, invariant: Boolean): Event[S, Any, Any] =
//    decl.getEvent(this, slot)
//}
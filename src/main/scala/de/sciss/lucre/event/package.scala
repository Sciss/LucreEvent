package de.sciss.lucre

import collection.immutable.{IndexedSeq => IIdxSeq}

package object event {
   type Reaction  = () => () => Unit
   private[event] type Children[ S <: stm.Sys[ S ]] = IIdxSeq[ (Int, Selector[ S ])]

   private val emptySeq = IIdxSeq.empty[ Nothing ]

//   private[lucre] def NoSources[ S <: Sys[ S ]]  : Sources[ S ]   = emptySeq
   private[lucre] def NoChildren[ S <: stm.Sys[ S ]] : Children[ S ]  = emptySeq

   type Serializer[ S <: stm.Sys[ S ], Repr ] = Reader[ S, Repr ] with stm.Serializer[ S#Tx, S#Acc, Repr ]
}
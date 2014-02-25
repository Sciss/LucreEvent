/*
 *  Trigger.scala
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

package de.sciss.lucre
package event

object Trigger {
   def apply[ S <: Sys[ S ], A ]( implicit tx: S#Tx ) : Standalone[ S, A ] = impl.TriggerImpl.apply[ S, A ]

   object Standalone {
      implicit def serializer[ S <: Sys[ S ], A ] : NodeSerializer[ S, Standalone[ S, A ]] =
         impl.TriggerImpl.standaloneSerializer[ S, A ]
   }
   trait Standalone[ S <: Sys[ S ], A ] extends Trigger[ S, A, Standalone[ S, A ]] with Node[ S ]
}

/**
 * A `Trigger` event is one which can be publically fired. One can think of it as the
 * imperative event in EScala.
 */
trait Trigger[ S <: stm.Sys[ S ], A, +Repr ] extends Event[ S, A, Repr ] {
   def apply( update: A )( implicit tx: S#Tx ) : Unit
}

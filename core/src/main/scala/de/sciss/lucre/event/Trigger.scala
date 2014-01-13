/*
 *  Trigger.scala
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

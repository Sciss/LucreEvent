package de.sciss.lucre
package event
package impl

import io.DataInput

object TriggerImpl {
   def apply[ S <: Sys[ S ], A ]( implicit tx: S#Tx ) : Trigger.Standalone[ S, A ] = new Standalone[ S, A ] {
      protected val targets = Targets[ S ]
   }

   def standaloneSerializer[ S <: Sys[ S ], A ] : NodeSerializer[ S, Trigger.Standalone[ S, A ]] =
      anyStandaloneSer.asInstanceOf[ NodeSerializer[ S, Trigger.Standalone[ S, A ]]]

   private val anyStandaloneSer = new StandaloneSerializer[ InMemory, Any ]

   private final class StandaloneSerializer[ S <: Sys[ S ], A ]
   extends NodeSerializer[ S, Trigger.Standalone[ S, A ]] {
      def read( in: DataInput, access: S#Acc, _targets: Targets[ S ])( implicit tx: S#Tx ) : Trigger.Standalone[ S, A ] =
         new Standalone[ S, A ] {
            protected val targets = _targets
         }
   }

   trait Standalone[ S <: Sys[ S ], A ] extends Trigger.Standalone[ S, A ]
   with TriggerImpl[ S, A, Trigger.Standalone[ S, A ]]
   with StandaloneLike[ S, A, Trigger.Standalone[ S, A ]] with Singleton[ S ] /* with EarlyBinding[ S, A ] */
   with Root[ S, A ] {
      final protected def reader: Reader[ S, Trigger.Standalone[ S, A ]] = standaloneSerializer[ S, A ]
   }
}
trait TriggerImpl[ S <: Sys[ S ], A, +Repr ]
extends Trigger[ S, A, Repr ] with EventImpl[ S, A, Repr ]
with Generator[ S, A, Repr ] {
   final def apply( update: A )( implicit tx: S#Tx ) { fire( update )}
}


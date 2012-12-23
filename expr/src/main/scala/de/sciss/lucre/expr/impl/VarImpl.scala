package de.sciss.lucre
package expr
package impl

import stm.Sys
import event.{Pull, Event, InvariantSelector, Change, impl => evti}
//import de.sciss.lucre.LucreSTM._

trait VarImpl[ S <: event.Sys[ S ], A ] extends Expr.Var[ S, A ]
with evti.StandaloneLike[ S, Change[ A ], Expr[ S, A ]] /* with LateBinding[ S, Change[ A ]] */
with evti.Generator[ S, Change[ A ], Expr[ S, A ]] with InvariantSelector[ S ] {
   expr =>

   private type Ex = Expr[ S, A ]

   final def changed: Event[ S, Change[ A ], Expr[ S, A ]] = this // changedImp

   // ---- these need to be implemented by subtypes ----
   protected def ref: S#Var[ Ex ]
   protected def reader: event.Reader[ S, Expr[ S, A ]]

   final protected def writeData( out: DataOutput ) {
      out.writeUnsignedByte( 0 )
      ref.write( out )
   }

   final protected def disposeData()( implicit tx: S#Tx ) {
      ref.dispose()
   }

   final private[lucre] def connect()( implicit tx: S#Tx ) {
      ref.get.changed ---> this
   }
   final private[lucre] def disconnect()( implicit tx: S#Tx ) {
      ref.get.changed -/-> this
   }

   final def get( implicit tx: S#Tx ) : Ex = ref.get
   final def set( expr: Ex )( implicit tx: S#Tx ) {
      val before = ref.get
      if( before != expr ) {
         val con = targets.nonEmpty
//         event.log( this.toString + " set " + expr + " (con = " + con + ")" )
         if( con ) before.changed -/-> this
         ref.set( expr )
         if( con ) {
            expr.changed ---> this
            val beforeV = before.value
            val exprV   = expr.value
            fire( Change( beforeV, exprV ))
         }
      }
   }

   final def transform( f: Ex => Ex )( implicit tx: S#Tx ) { set( f( get ))}

//   final def isFresh( implicit tx: S#Tx ) : Boolean = ref.isFresh

//      final def getFresh( implicit tx: S#Tx ) : Ex = ref.getFresh

   final def value( implicit tx: S#Tx ) : A = ref.get.value

   final def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Change[ A ]] = {
      if( pull.parents( this /* select() */).isEmpty ) {
         pull.resolve[ Change[ A ]]
      } else {
         get.changed.pullUpdate( pull )
      }
   }

   override def toString() = "Expr.Var" + id
}

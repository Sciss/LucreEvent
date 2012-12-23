/*
 *  Expr.scala
 *  (LucreExpr)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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
package expr

import stm.{Disposable, Var => _Var, Sys}
import event.{EventLike, Dummy, Event, Change}

object Expr {
   trait Node[ S <: Sys[ S ], A ] extends Expr[ S, A ] with event.Node[ S ] {
      def changed: Event[ S, Change[ A ], Expr[ S, A ]]
   }

   object Var {
      def unapply[ S <: Sys[ S ], A ]( expr: Expr[ S, A ]) : Option[ Var[ S, A ]] = {
         if( expr.isInstanceOf[ Var[ _, _ ]]) Some( expr.asInstanceOf[ Var[ S, A ]]) else None
      }
   }
   trait Var[ S <: Sys[ S ], A ] extends Expr[ S, A ] with _Var[ S#Tx, Expr[ S, A ]] {
      def changed: Event[ S, Change[ A ], Expr[ S, A ]]
   }

   object Const {
      def unapply[ S <: Sys[ S ], A ]( expr: Expr[ S, A ]) : Option[ A ] = {
         if( expr.isInstanceOf[ Const[ _, _ ]]) {
            Some( expr.asInstanceOf[ Const[ S, A ]].constValue )
         } else None
      }
   }
   trait Const[ S <: Sys[ S ], A ] extends Expr[ S, A ] {
      final def changed = Dummy[ S, Change[ A ], Expr[ S, A ]]
      protected def constValue : A
      final def value( implicit tx: S#Tx ) : A = constValue
      override def toString = constValue.toString
      final def dispose()( implicit tx: S#Tx ) {}
   }
   def isConst( expr: Expr[ _, _ ]) : Boolean = expr.isInstanceOf[ Const[ _, _ ]]
}

trait Expr[ S <: Sys[ S ], A ] extends Writable with Disposable[ S#Tx ] {
   def changed: EventLike[ S, Change[ A ], Expr[ S, A ]]
   def value( implicit tx: S#Tx ) : A

   final def observe( fun: A => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] =
      observeTx( _ => fun )

   final def observeTx( fun: S#Tx => A => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] = {
      val o = changed.reactTx[ Change[ A ]] { tx => change => fun( tx )( change.now )}
      fun( tx )( value )
      o
   }
}

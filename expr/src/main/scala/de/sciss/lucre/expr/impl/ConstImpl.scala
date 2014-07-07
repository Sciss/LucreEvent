/*
 *  ConstImpl.scala
 *  (LucreEvent)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package expr
package impl
import event.{impl => eimpl, Sys}

trait ConstImpl[S <: Sys[S], A] extends Expr.Const[S, A] with eimpl.Constant

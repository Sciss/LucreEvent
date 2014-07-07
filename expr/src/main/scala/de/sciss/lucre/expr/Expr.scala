/*
 *  Expr.scala
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

package de.sciss
package lucre
package expr

import stm.{Disposable, Var => _Var}
import de.sciss.lucre.event.{Publisher, Dummy, Event, Sys}
import serial.Writable
import de.sciss.model.Change
import expr.{Boolean => _Boolean, String => _String}

object Expr {
  trait Node[S <: Sys[S], +A] extends Expr[S, A] with event.Node[S] {
    def changed: Event[S, Change[A], Expr[S, A]]
  }

  object Var {
    def unapply[S <: Sys[S], A](expr: Expr[S, A]): Option[Var[S, A]] = {
      if (expr.isInstanceOf[Var[_, _]]) Some(expr.asInstanceOf[Var[S, A]]) else None
    }
  }

  trait Var[S <: Sys[S], A] extends Node[S, A] with _Var[S#Tx, Expr[S, A]]

  object Const {
    def unapply[S <: Sys[S], A](expr: Expr[S, A]): Option[A] = {
      if (expr   .isInstanceOf[Const[_, _]]) {
        Some(expr.asInstanceOf[Const[S, A]].constValue)
      } else None
    }
  }

  trait Const[S <: Sys[S], +A] extends Expr[S, A] {
    final def changed = Dummy[S, Change[A]]

    protected def constValue: A
    final def value(implicit tx: S#Tx): A = constValue

    override def toString = constValue.toString

    final def dispose()(implicit tx: S#Tx) = ()
  }

  def isConst(expr: Expr[_, _]): Boolean = expr.isInstanceOf[Const[_, _]]
}

trait Expr[S <: Sys[S], +A] extends Writable with Disposable[S#Tx] with Publisher[S, Change[A]] {
  def value(implicit tx: S#Tx): A

  //  final def observe(fun: A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
  //    observeTx(_ => fun)
  //
  //  final def observeTx(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
  //    val o = changed.react { tx => change =>
  //      fun(tx)(change.now)
  //    }
  //    fun(tx)(value)
  //    o
  //  }
}

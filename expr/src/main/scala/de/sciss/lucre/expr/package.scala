package de.sciss.lucre

import language.implicitConversions

package object expr {
  /**
   * The no-op method shadows the crappy `scala.Predef.any2stringadd` which prevents
   * any DSL from using the plus operator.
   */
  implicit def any2stringadd(x: Any): Any = x
}
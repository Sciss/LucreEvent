package de.sciss.lucre.event

trait Publisher[S <: Sys[S], +A] {
  def changed: EventLike[S, A]
}

package org.enso.data

sealed trait Compare
object Compare {
  case object LT extends Compare
  case object GT extends Compare
  case object EQ extends Compare
  def compare[T: Ordering](a: T, b: T): Compare = {
    if (implicitly[Ordering[T]].lt(a, b)) LT
    else if (implicitly[Ordering[T]].gt(a, b)) GT
    else EQ
  }
}

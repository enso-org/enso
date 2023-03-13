package org.enso.data

/** Strongly typed size for a container. */
case class Size(value: Int) extends AnyVal with Ordered[Size] {
  def +(offset: Size): Size   = Size(value + offset.value)
  def compare(rhs: Size): Int = value compare rhs.value
}

object Size {
  val Empty                     = Size(0)
  def apply(text: String): Size = Size(text.length)
}

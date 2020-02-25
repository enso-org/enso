package org.enso.data

/** Strongly typed index position in a container. */
case class Index(value: Int) extends AnyVal with Ordered[Index] {
  def +(offset: Size): Index = Index(value + offset.value)
  def -(offset: Size): Index = Index(value - offset.value)

  /** Span between two text positions. Operands order is irrelevant. */
  def <->(that: Index): Span =
    if (value <= that.value) Span(this, Size(that.value - value))
    else Span(that, Size(value - that.value))

  def asSize: Size = Size(value)

  def compare(rhs: Index): Int = value compare rhs.value
}

object Index {
  val Start = Index(0)
}

package org.enso.data

/** Strongly typed index position in a container. */
case class Index(index: Int) extends AnyVal with Ordered[Index] {
  def +(offset: Size): Index = Index(index + offset.value)
  def -(offset: Size): Index = Index(index - offset.value)

  /** Span between two text positions. Operands order is irrelevant. */
  def <->(that: Index): Span =
    if (index <= that.index) Span(this, Size(that.index - index))
    else Span(that, Size(index - that.index))

  def asSize: Size = Size(index)

  def compare(rhs: Index): Int = index compare rhs.index
}

object Index {
  val Start = Index(0)
}

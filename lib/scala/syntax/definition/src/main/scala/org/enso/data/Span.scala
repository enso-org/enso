package org.enso.data

/** Strongly typed span in a container. */
case class Span(index: Index, size: Size) extends Ordered[Span] {

  override def compare(that: Span): Int =
    (index -> that.size).compare(that.index -> size)

  /** Index of the first element past the span */
  def end: Index = index + size

}

object Span {
  def apply(text: String): Span =
    Span(Index.Start, Size(text))

  def Empty(pos: Index): Span = Span(pos, Size.Empty)

  implicit class StringOps(text: String) {
    def substring(span: Span): String =
      text.substring(span.index.value, span.end.value)
  }
}

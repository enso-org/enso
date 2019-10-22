package org.enso.data

import org.enso.syntax.text.AST

/** Strongly typed span in a container. */
case class Span(begin: Index, length: Size) extends Ordered[Span] {

  override def compare(that: Span): Int =
    (begin -> that.length).compare(that.begin -> length)

  /** Index of the first element past the span */
  def end: Index = begin + length

}

object Span {

  def apply(pos: Index, ast: AST): Span =
    Span(pos, Size(ast.span))

  def apply(text: String): Span =
    Span(Index.Start, Size(text))

  def Empty(pos: Index): Span = Span(pos, Size.Empty)

  implicit class StringOps(text: String) {
    def substring(span: Span): String =
      text.substring(span.begin.index, span.end.index)
  }
}

package org.enso.data

import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.meta.Pattern

/** Strongly typed size for a container. */
case class Size(value: Int) extends AnyVal with Ordered[Size] {
  def +(offset: Size): Size   = Size(value + offset.value)
  def compare(rhs: Size): Int = value compare rhs.value
}

object Size {
  val Empty                           = Size(0)
  def apply(pat: Pattern.Match): Size = Size(pat.toStream)
  def apply(ast: AST): Size           = Size(ast.span)
  def apply(text: String): Size       = Size(text.length)
  def apply(ast: Shifted[AST]): Size  = Size(ast.off) + Size(ast.el)
  def apply[A](elems: Seq[Shifted[AST]]): Size = {
    var ret = Size(0)
    elems.foreach(ret += Size(_))
    ret
  }
}

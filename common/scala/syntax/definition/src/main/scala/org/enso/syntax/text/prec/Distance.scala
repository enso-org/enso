package org.enso.syntax.text.prec

import org.enso.syntax.text.AST
import org.enso.data.List1
import org.enso.data.Shifted
import scala.annotation.tailrec

object Distance {

  /** Segment is a list of AST tokens which are not separated with spaces */
  type Segment = List1[AST]

  def partition(lst: List1[Shifted[AST]]): List1[Shifted[Segment]] = {
    @tailrec
    def go(
      input: List[Shifted[AST]],
      currentOff: Int,
      current: List1[AST],
      out: List[Shifted[Segment]]
    ): List1[Shifted[Segment]] = input match {
      case Nil => List1(Shifted(currentOff, current.reverse), out).reverse
      case ast1 :: ast2_ =>
        val isBlock = ast1.wrapped match {
          case AST.Block.any(_) => true
          case _                => false
        }
        val isGlued = (ast1.off == 0) && (!isBlock)
        isGlued match {
          case true => go(ast2_, currentOff, ast1.wrapped :: current, out)
          case false =>
            val out2 = Shifted(currentOff, current.reverse) :: out
            go(ast2_, ast1.off, List1(ast1.wrapped), out2)
        }
    }
    go(lst.tail, lst.head.off, List1(lst.head.wrapped), Nil)
  }
}

package org.enso.syntax.text

import scala.annotation.tailrec

/**
  * Useful debugging tools for the parser AST.
  */
object Debug {

  /**
    * Pretty prints the parser AST string representation in a more readable
    * format.
    *
    * @param str the string representation of the parser AST
    * @return the pretty-printed version of `str`
    */
  def pretty(str: String): String = {

    def checkClosing(in: List[Char]): Int = {
      @tailrec
      def go(i: Int, rest: Int, in: List[Char], bias: Int): Int =
        (rest, bias, in) match {
          case (0, _, _)   => 0
          case (_, 0, _)   => i
          case (_, _, Nil) => i
          case (_, _, s :: ss) =>
            s match {
              case '(' => go(i + 1, rest - 1, ss, bias - 1)
              case ')' => go(i + 1, rest - 1, ss, bias + 1)
              case _   => go(i + 1, rest - 1, ss, bias)
            }

        }
      go(0, 10, in, -1)
    }

    @tailrec
    def go(ind: Int, in: List[Char], out: List[String]): List[String] = {
      def newline(i: Int) = "\n" + " " * i * 2
      in match {
        case Nil => out
        case s :: ss =>
          val s2 = s.toString
          s match {
            case '(' =>
              checkClosing(ss) match {
                case 0 => go(ind + 1, ss, newline(ind + 1) :: s2 :: out)
                case i =>
                  go(
                    ind,
                    ss.drop(i),
                    ss.take(i).mkString("") :: s2 :: out
                  )
              }

            case ')' => go(ind - 1, ss, s2 :: newline(ind - 1) :: out)
            case ',' => if (ss.startsWith(" ")) {
              go(ind, ss.drop(1), newline(ind) :: s2 :: out)
            } else {
              go(ind, ss, newline(ind) :: s2 :: out)
            }
            case _   => go(ind, ss, s2 :: out)
          }
      }
    }
    go(0, str.toList, List()).reverse.mkString("")
  }
}

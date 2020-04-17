package org.enso.flexer.automata

import org.enso.flexer.Parser
import scala.annotation.tailrec

trait Pattern {
  import Pattern._

  def |(that: Pattern): Pattern  = Or(this, that)
  def >>(that: Pattern): Pattern = Seq(this, that)
  def many: Pattern              = Many(this)
  def many1: Pattern             = this >> many
  def opt: Pattern               = this | always
}

object Pattern {
  case object Always                              extends Pattern
  case class Range(start: Int, end: Int)          extends Pattern
  case class Or(left: Pattern, right: Pattern)    extends Pattern
  case class Seq(first: Pattern, second: Pattern) extends Pattern
  case class Many(body: Pattern)                  extends Pattern

  //// API ////

  val always: Pattern                      = Always
  def range(start: Char, end: Char): Range = Range(start.toInt, end.toInt)
  def range(start: Int, end: Int): Range   = Range(start, end)
  def range(end: Int): Range               = range(0, end)
  def range(end: Char): Range              = range(0, end.toInt)
  def char(char: Char): Range              = range(char.toInt, char.toInt)
  def char(char: Int): Range               = range(char, char)

  val never: Pattern = range(-1)
  val any: Range     = range(Int.MaxValue)
  val eof: Range     = char(Parser.eofCodePoint)

  def anyOf(chars: String): Pattern            = anyOf(chars.map(char))
  def anyOf(alts: scala.Seq[Pattern]): Pattern = alts.fold(never)(_ | _)
  def noneOf(chars: String): Pattern = {
    val pointCodes  = chars.map(_.toInt).sorted
    val startPoints = 0 +: pointCodes.map(_ + 1)
    val endPoints   = pointCodes.map(_ - 1) :+ Int.MaxValue
    val ranges      = startPoints.zip(endPoints)
    val validRanges = ranges.filter { case (s, e) => e >= s }
    val patterns    = validRanges.map { case (s, e) => range(s, e) }
    anyOf(patterns)
  }

  final def not(char: Char): Pattern =
    noneOf(char.toString)

  def repeat(pat: Pattern, min: Int, max: Int): Pattern = {
    @tailrec
    def go(i: Int, ch: Pattern, out: Pattern): Pattern =
      i match {
        case 0 => out
        case _ =>
          val ch2 = ch >> pat
          go(i - 1, ch2, out | ch2)
      }
    val minPat = repeat(pat, min)
    go(max - min, minPat, minPat)
  }

  def repeat(pat: Pattern, num: Int): Pattern =
    0.until(num).foldLeft(always)((t, _) => t >> pat)

  //// Implicits ////

  implicit class ExtendedChar(_this: Char) {
    final def ||(that: Char): Pattern =
      Or(char(_this), char(that))
  }

  implicit def automataPtternFromChar(char: Char): Pattern =
    range(char, char)
  implicit def automataPatternFromString(str: String): Pattern =
    str.toList match {
      case Nil     => always
      case s :: ss => ss.foldLeft(char(s): Pattern)(_ >> _)
    }
}

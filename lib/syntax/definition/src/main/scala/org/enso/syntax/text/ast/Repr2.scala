package org.enso.syntax.text.ast2

//// README ////
//
// This is a WORK IN PROGRESS in-place replacement for Repr implementation.
// It is meant to lower the memory consumption of AST by removing Repr cache
// from AST and keeping only the span information. In order to do so, we have
// to do the following steps:
// 1. ASTOf should keep val span:Int
// 2. ASTOf should keep def repr instead of val repr
// 3. ASTOf should implement the Spanned type class
// 4. In every definition of AST Shape we should annotate the Child to be both
//    Spanned as well as Repr. Otherwise, the Spanned instance will be
//    automatically created from Repr. See below for explanation.
//
// This implementation works even if the above points are not done, because
// Spanned implementation fallbacks to Repr when needed. However, this may not
// be the best design as its very error prone. If we provide `Repr` as super
// class constraint, the `Spanned` is auto-derived, which may make the code
// run very slow.
//
// Anyway, before moving to this implementation we need to solve another
// problem. Printing of the AST throws stackoverflow here, as we are not caching
// everything in memory anymore, but we are recursively printing each component,
// and for a deeply nested components (like 100k open braces) we reach the limit
// of the recursion in the lambda created inside of [[Repr.Builder.+]]
// implementation.
//
//      def newBuiltMe(bldr: StringBuilder): Unit = {
//        buildMe(bldr)
//        sr.repr.repr(that).buildMe(bldr)
//      }
//
// We should probably apply CPS transformation to the code builder here,
// unless there is any other better solution.

import cats.Monoid
import cats.implicits._
import org.enso.data.{List1, Shifted}

trait SpannedRepr[T] {
  val spanned: Spanned[T]
  val repr: Repr[T]
}
object SpannedRepr {
  def apply[T: SpannedRepr]: SpannedRepr[T] = implicitly[SpannedRepr[T]]

  implicit def auto[T: Repr: Spanned]: SpannedRepr[T] = new SpannedRepr[T] {
    val spanned = implicitly[Spanned[T]]
    val repr    = implicitly[Repr[T]]
  }
  implicit def asSpanned[T: SpannedRepr]: Spanned[T] = SpannedRepr[T].spanned
  implicit def asRepr[T: SpannedRepr]:    Repr[T]    = SpannedRepr[T].repr
}

trait Spanned[T] {
  def span(t: T): Int
}
object Spanned {
  def apply[T: Spanned](t: T): Int = implicitly[Spanned[T]].span(t)

  implicit def spannedForInt: Spanned[Int] = identity(_)
  implicit def spannedForList[T: Spanned]: Spanned[List[T]] =
    _.map(Spanned(_)).sum
  implicit def spannedForList1[T: Spanned]: Spanned[List1[T]] =
    _.map(Spanned(_)).foldLeft(0)(_ + _)
  implicit def spannedForShifted[T: Spanned]: Spanned[Shifted[T]] =
    t => t.off + Spanned(t.wrapped)
  implicit def spannedForRepr[T: Repr]: Spanned[T] = t => Repr(t).span

}

////////////////////////////////////////////////////////////////////////////////
//// Repr //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

trait Repr[T] {
  def repr(a: T): Repr.Builder
}
object Repr {

  //// Smart Constructors ////

  def apply[T: Repr](t: T): Builder = implicitly[Repr[T]].repr(t)
  val R = Monoid[Builder].empty

  //// Operations ////

  implicit class ToReprOps[T: Repr](t: T) {
    def repr: Builder = Repr(t)
    def span: Int     = repr.span
  }

  ///// Instances ////

  implicit def reprForUnit: Repr[Unit] =
    _ => Monoid[Builder].empty
  implicit def reprForString: Repr[String] =
    Repr.Builder.Text(_)
  implicit def reprForInt: Repr[Int] = {
    case 0 => R
    case i => Repr.Builder.Space(i)
  }
  implicit def reprForChar: Repr[Char] =
    Repr.Builder.Letter(_)
  implicit def reprForTuple2[T1: Repr, T2: Repr]: Repr[(T1, T2)] =
    t => Repr(t._1) |+| Repr(t._2)
  implicit def reprForProvider[T <: Repr.Provider]: Repr[T] =
    _.repr
  implicit def reprForList[T: Repr]: Repr[List[T]] =
    _.map(_.repr).fold(R: Builder)(Repr.Builder.Seq)
  implicit def reprForList1[T: Repr: Spanned]: Repr[List1[T]] =
    t => R + t.head + t.tail
  implicit def reprForShifted[T: Repr: Spanned]: Repr[Shifted[T]] =
    t => R + t.off + t.wrapped
  implicit def reprForShiftedList1[T: Repr: Spanned]: Repr[Shifted.List1[T]] =
    t => R + t.head + t.tail
  implicit def reprForOption[T: Repr]: Repr[Option[T]] =
    _.map(_.repr).getOrElse(R)
  implicit def reprForNone: Repr[None.type] =
    _ => R
  implicit def reprForSome[T: Repr]: Repr[Some[T]] =
    _.map(_.repr).getOrElse(R)

  //////////////////////////////////////////////////////////////////////////////
  //// Provider ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  trait Provider {
    val repr: Builder
  }

  case class Builder(span: Int, buildMe: StringBuilder => Unit) {

    def build(): String = {
      val bldr = new StringBuilder()
      buildMe(bldr)
      bldr.result()
    }

    def +[T: SpannedRepr](that: T) = {
      val sr      = SpannedRepr[T]
      val newSpan = span + sr.spanned.span(that)
      def newBuiltMe(bldr: StringBuilder): Unit = {
        buildMe(bldr)
        sr.repr.repr(that).buildMe(bldr)
      }
      Builder(newSpan, newBuiltMe)
    }

    def ++[T: Repr](that: T): Builder = this + " " + that
  }
  object Builder {
    implicit val monoidForBuilder: Monoid[Builder] = new Monoid[Builder] {
      def empty: Builder = Empty()
      def combine(l: Builder, r: Builder): Builder =
        Builder(l.span + r.span, (bldr: StringBuilder) => {
          l.buildMe(bldr)
          r.buildMe(bldr)
        })
    }

    def Empty()                     = Builder(0, identity(_): Unit)
    def Space(span: Int)            = Builder(span, _ ++= (" " * span))
    def Letter(char: Char)          = Builder(1, _ += char)
    def Text(str: String)           = Builder(str.length, _ ++= str)
    def Seq(l: Builder, r: Builder) = l |+| r

    implicit def fromString(a: String):        Builder = Repr(a)
    implicit def fromChar(a: Char):            Builder = Repr(a)
    implicit def reprForBuilder[T <: Builder]: Repr[T] = identity(_)
  }
}

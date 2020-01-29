package org.enso.syntax.text.ast3

//// README ////
//
// SEE THE README OF Repr2.scala first.
//
// Here is a simple Repr implementation with a trampoline in Shape._Seq.

import java.nio.charset.StandardCharsets

import org.enso.data.List1
import org.enso.data.Shifted
import cats.Monoid
import cats.implicits._

import scala.annotation.tailrec

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

  case class Builder(span: Int, shape: () => Shape) {

    def build(): String = shape().build()

    def +[T: SpannedRepr](that: T) = {
      val sr      = SpannedRepr[T]
      val newSpan = span + sr.spanned.span(that)
      def newShape(): Shape = Shape._Seq(shape, sr.repr.repr(that).shape)

      Builder(newSpan, newShape)
    }

    def ++[T: Repr](that: T): Builder = this + " " + that
  }
  object Builder {
    implicit val monoidForBuilder: Monoid[Builder] = new Monoid[Builder] {
      def empty: Builder = Empty()
      def combine(l: Builder, r: Builder): Builder =
        Builder(
          l.span + r.span,
          () => Shape._Seq(() => l.shape(), () => r.shape())
        )
    }

    def Empty()                     = Builder(0, () => Shape.Empty())
    def Space(span: Int)            = Builder(span, () => Shape.Space(span))
    def Letter(char: Char)          = Builder(1, () => Shape.Letter(char))
    def Text(str: String)           = Builder(str.length, () => Shape.Text(str))
    def Seq(l: Builder, r: Builder) = l |+| r

    implicit def fromString(a: String):        Builder = Repr(a)
    implicit def fromChar(a: Char):            Builder = Repr(a)
    implicit def reprForBuilder[T <: Builder]: Repr[T] = identity(_)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Shape ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Shape {
    import Shape._

//    val byteSpan: Int
//    val span: Int

    def +[T: Repr](that: T):  Shape = Seq(this, Repr(that).shape())
    def ++[T: Repr](that: T): Shape = this + " " + that
    def build(): String = {
      val bldr = new StringBuilder()
      @tailrec
      def go(lst: List[Shape]): Unit = lst match {
        case Nil =>
        case r :: rs =>
          r match {
            case _: Empty  => go(rs)
            case r: Letter => bldr += r.char; go(rs)
            case r: Space  => for (_ <- 1 to r.span) { bldr += ' ' }; go(rs)
            case r: Text   => bldr ++= r.str; go(rs)
            case r: _Seq   => go(r.first() :: r.second() :: rs)
          }
      }
      go(List(this))
      bldr.result()
    }
  }
  object Shape {

    //// Constructors ////

    final case class Empty() extends Shape {
      val byteSpan = 0
      val span     = 0
    }
    final case class Letter(char: Char) extends Shape {
      val byteSpan = char.toString.getBytes(StandardCharsets.UTF_8).length
      val span     = 1
    }
    final case class Space(span: Int) extends Shape {
      val byteSpan = span
    }
    final case class Text(str: String) extends Shape {
      val byteSpan = str.getBytes(StandardCharsets.UTF_8).length
      val span     = str.length
    }
    final case class _Seq(first: () => Shape, second: () => Shape)
        extends Shape {
//      val byteSpan = first.byteSpan + second.byteSpan
//      val span     = first.span + second.span
    }
    object Seq { def apply(l: Shape, r: Shape): Shape = l |+| r }

    //// Instances ////

    implicit val monoidForShape: Monoid[Shape] = new Monoid[Shape] {
      def empty: Shape = Empty()
      def combine(l: Shape, r: Shape): Shape = (l, r) match {
        case (_: Empty, t) => t
        case (t, _: Empty) => t
        case _             => _Seq(() => l, () => r)
      }
    }
  }
}

package org.enso.syntax.text.ast

import java.nio.charset.StandardCharsets

import org.enso.data.List1
import org.enso.data.Shifted
import cats.Monoid
import cats.implicits._

import scala.annotation.tailrec

////////////////////////////////////////////////////////////////////////////////
//// Repr //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

trait Repr[T] {
  def repr(a: T): Repr.Builder
}
object Repr {

  //// Smart Constructors ////

  def apply[T: Repr](t: T): Builder = implicitly[Repr[T]].repr(t)

  val R = Repr.Builder.Empty()

  //// Operations ////

  implicit class ToReprOps[T: Repr](t: T) {
    def repr: Builder = Repr(t)
  }

  ///// Instances ////

  implicit def reprForUnit: Repr[Unit] =
    _ => Repr.Builder.Empty()
  implicit def reprForString: Repr[String] =
    Repr.Builder.Text(_)
  implicit def reprForInt: Repr[Int] = {
    case 0 => R
    case i => Repr.Builder.Space(i)
  }
  implicit def reprForChar: Repr[Char] =
    Repr.Builder.Letter(_)
  implicit def reprForTuple2[T1: Repr, T2: Repr]: Repr[(T1, T2)] =
    t => Repr.Builder.Seq(Repr(t._1), Repr(t._2))
  implicit def reprForProvider[T <: Repr.Provider]: Repr[T] =
    _.repr
  implicit def reprForList[T: Repr]: Repr[List[T]] =
    _.map(_.repr).fold(R: Builder)(Repr.Builder.Seq(_, _))
  implicit def reprForList1[T: Repr]: Repr[List1[T]] =
    t => R + t.head + t.tail
  implicit def reprForShifted[T: Repr]: Repr[Shifted[T]] =
    t => R + t.off + t.wrapped
  implicit def reprForShiftedList1[T: Repr]: Repr[Shifted.List1[T]] =
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

  //////////////////////////////////////////////////////////////////////////////
  //// Builder /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Builder {
    import Builder._

    val byteSpan: Int
    val span: Int

    def +[T: Repr](that: T): Builder = this |+| Repr(that)
    def build(): String = {
      val bldr = new StringBuilder()
      @tailrec
      def go(lst: List[Builder]): Unit = lst match {
        case Nil =>
        case r :: rs =>
          r match {
            case _: Empty  => go(rs)
            case r: Letter => bldr += r.char; go(rs)
            case r: Space  => for (_ <- 1 to r.span) { bldr += ' ' }; go(rs)
            case r: Text   => bldr ++= r.str; go(rs)
            case r: Seq    => go(r.first :: r.second :: rs)
          }
      }
      go(List(this))
      bldr.result()
    }
  }
  object Builder {

    //// Constructors ////

    final case class Empty() extends Builder {
      val byteSpan = 0
      val span     = 0
    }
    final case class Letter(char: Char) extends Builder {
      val byteSpan = char.toString.getBytes(StandardCharsets.UTF_8).length
      val span     = 1
    }
    final case class Space(span: Int) extends Builder {
      val byteSpan = span
    }
    final case class Text(str: String) extends Builder {
      val byteSpan = str.getBytes(StandardCharsets.UTF_8).length
      val span     = str.length
    }
    final case class Seq(first: Builder, second: Builder) extends Builder {
      val byteSpan = first.byteSpan + second.byteSpan
      val span     = first.span + second.span
    }

    //// Instances ////

    implicit def fromString(a: String):        Builder = Repr(a)
    implicit def fromChar(a: Char):            Builder = Repr(a)
    implicit def reprForBuilder[T <: Builder]: Repr[T] = identity(_)
    implicit val monoidForBuilder: Monoid[Builder] = new Monoid[Builder] {
      def empty: Builder = R
      def combine(l: Builder, r: Builder): Builder = (l, r) match {
        case (_: Empty, t) => t
        case (t, _: Empty) => t
        case _             => Seq(l, r)
      }
    }
  }

}

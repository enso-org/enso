package org.enso.syntax.text

import java.util.UUID

import cats.{Foldable, Functor, Monoid}
import cats.derived._
import cats.implicits._
import io.circe.{Encoder, Json}
import org.enso.data.List1._
import org.enso.data._
import org.enso.syntax.text.HasSpan.implicits._
import org.enso.syntax.text.ast.Repr.{R, _}
import org.enso.syntax.text.ast.meta.Pattern
import org.enso.syntax.text.ast.{opr, Doc, Repr}
import org.enso.syntax.text.ast.text.{Escape, RawEscape}

import scala.annotation.{nowarn, tailrec, unused}
import scala.reflect.ClassTag

/* Note [JSON Serialization]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Using Circe's auto-derived `asJson` on AST is extremely costly in terms
 * of compile-time resource usage. It adds like 2-4 min to compile time.
 * For that reason we should only have one place where it is used and other
 * places where AST needs to be serialized should use this wrapper.
 *
 * Also, it must be placed in this package having it separate for some reason
 * increases compile-time memory usage, causing CI builds to fail. Someone might
 * want to reinvestigate this in future.
 *
 * Also, this function definition can't be just "anywhere" in the file, but
 * near bottom to "properly" see other things.
 *
 * When working on this file, it is recommended to temporarily replace
 * function body with ??? expression to radically improve compiler throughput.
 *
 * Also note that JSON serialization is meant to be kept synchronized with Rust
 * AST implementation. Any changes made to serialization here (that includes
 * changes to case classes' names and field names) need to be kept in sync with
 * the Rust side.
 */

//////////////////////////////////////////////////////////////////////////////
//// HasSpan /////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

trait HasSpan[T] {
  def span(t: T): Int
}
object HasSpan {
  def apply[T: HasSpan]: HasSpan[T] = implicitly[HasSpan[T]]

  object implicits {
    implicit class ToHasSpanOps[T: HasSpan](t: T) {
      def span(): Int = {
        implicitly[HasSpan[T]].span(t)
      }
    }
  }

  implicit def fromShifted[T: HasSpan]: HasSpan[Shifted[T]] = { shifted =>
    val ev = implicitly[HasSpan[T]]
    shifted.off + ev.span(shifted.wrapped)
  }

  implicit def fromOption[T: HasSpan]: HasSpan[Option[T]] =
    opt => opt.map(_.span()).getOrElse(0)
  implicit def fromList[T: HasSpan]: HasSpan[List[T]] =
    list => list.map(_.span()).sum
  implicit def fromList1[T: HasSpan]: HasSpan[List1[T]] =
    list => list.toList.span()
  implicit def fromShiftedList1[T: HasSpan]: HasSpan[Shifted.List1[T]] =
    list => list.head.span() + list.tail.span()
}

//////////////////////////////////////////////////////////////////////////////
//// OffsetZip ///////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/** Zips every child [[A]] with offset from the left side of the parent
  * node. The offset is a number of UTF-8 code points.
  */
trait OffsetZip[F[A], A] {
  def zipWithOffset(t: F[A]): F[(Index, A)]
}
object OffsetZip {
  def apply[F[A], A](implicit ev: OffsetZip[F, A]): OffsetZip[F, A] = ev
  def apply[F[A], A](t: F[A])(implicit ev: OffsetZip[F, A]): F[(Index, A)] =
    OffsetZip[F, A].zipWithOffset(t)

  //// Default Instances ////
  implicit def fromStream[T: HasSpan]: OffsetZip[AST.StreamOf, T] = { stream =>
    val ev  = implicitly[HasSpan[T]]
    var off = Index.Start
    stream.map { t =>
      off += Size(t.off)
      val out = t.map((off, _))
      off += Size(ev.span(t.wrapped))
      out
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//// AbsolutePosition //////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/** Represents an expression's absolute positioning in a source file.
  * @param start the inclusive, 0-indexed position of the beginning
  *              of the expression
  * @param end the exclusive, 0-indexed position of the end of
  *            the expression
  */
case class Location(start: Int, end: Int) {
  def length: Int = end - start
}

object Location {
  implicit val optionSpanMonoid: Monoid[Option[Location]] =
    new Monoid[Option[Location]] {
      def empty: Option[Location] = None

      def combine(
        x: Option[Location],
        y: Option[Location]
      ): Option[Location] =
        x match {
          case None => y
          case Some(lSpan @ Location(lStart, _)) =>
            y match {
              case None => Some(lSpan)
              case Some(Location(_, rEnd)) =>
                Some(Location(lStart, rEnd))
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////
//// Phantom /////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/** Phantom type. Use with care, as Scala cannot prove its proper usage. When
  * a type is phantom, then its last type argument is not used and we can
  * safely coerce it to something else.
  */
sealed trait Phantom
object Phantom {
  implicit class PhantomOps[T[_] <: Phantom](ident: T[_]) {
    def coerce[S]: T[S] = ident.asInstanceOf[T[S]]
  }
}

//////////////////////////////////////////////////////////////////////////////
//// Shape ///////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

sealed trait Shape[T]

@nowarn("msg=parameter value evidence")
object Shape extends ShapeImplicit {
  import AST.StreamOf
  import HasSpan.implicits._

  /// Utils ///
  val newline = R + '\n'

  /////////////////
  //// Invalid ////
  /////////////////
  sealed trait Invalid[T]                       extends Shape[T]
  final case class Unrecognized[T](str: String) extends Invalid[T] with Phantom
  final case class Unexpected[T](msg: String, stream: StreamOf[T])
      extends Invalid[T]
  final case class DanglingBase[T](base: String) extends Invalid[T] with Phantom
  final case class TextUnclosed[T](line: TextLine[T])
      extends Text[T]
      with Invalid[T] {
    def quote = line.quote
  }
  final case class InvalidQuote[T](quote: Builder)
      extends Invalid[T]
      with Phantom
  final case class InlineBlock[T](quote: Builder)
      extends Invalid[T]
      with Phantom

  ///////////////////
  /// Identifiers ///
  ///////////////////
  sealed trait Ident[T] extends Shape[T] with Phantom { val name: String }

  final case class Blank[T]()            extends Ident[T] { val name = "_" }
  final case class Var[T](name: String)  extends Ident[T]
  final case class Cons[T](name: String) extends Ident[T]
  final case class Mod[T](name: String)  extends Ident[T]
  final case class Opr[T](name: String) extends Ident[T] {
    val (prec, assoc) = opr.Info.of(name)
  }
  final case class Annotation[T](name: String) extends Ident[T]
  final case class InvalidSuffix[T](elem: AST.Ident, suffix: String)
      extends Invalid[T]
      with Phantom

  ///////////////
  /// Literal ///
  ///////////////
  sealed trait Literal[T] extends Shape[T]

  //////////////
  /// Number ///
  //////////////
  final case class Number[T](base: Option[String], int: String)
      extends Literal[T]
      with Phantom

  ////////////
  /// Text ///
  ////////////
  sealed trait Text[T] extends Shape[T] with Literal[T] {
    def quote: Repr.Builder
  }

  /// Line ///
  sealed trait TextLine[T] extends Text[T]
  final case class TextLineRaw[T](text: List[SegmentRaw[T]])
      extends TextLine[T]
      with Phantom {
    val quote = '"'
  }
  final case class TextLineFmt[T](text: List[SegmentFmt[T]])
      extends TextLine[T] {
    val quote = '\''
  }

  /// Block ///
  sealed trait TextBlock[T] extends Text[T]
  final case class TextBlockLine[+T](empty_lines: List[Int], text: List[T])
  final case class TextBlockRaw[T](
    text: List[TextBlockLine[SegmentRaw[T]]],
    spaces: Int,
    offset: Int
  ) extends TextBlock[T]
      with Phantom {
    val quote = "\"\"\""
  }
  final case class TextBlockFmt[T](
    text: List[TextBlockLine[SegmentFmt[T]]],
    spaces: Int,
    offset: Int
  ) extends TextBlock[T] {
    val quote = "'''"
  }

  /// Segment ///
  sealed trait Segment[T]
  sealed trait SegmentFmt[T] extends Segment[T]
  sealed trait SegmentRaw[T] extends SegmentFmt[T] with Phantom

  final case class SegmentPlain[T](value: String)   extends SegmentRaw[T]
  final case class SegmentExpr[T](value: Option[T]) extends SegmentFmt[T]
  final case class SegmentEscape[T](code: Escape)
      extends SegmentFmt[T]
      with Phantom
  final case class SegmentRawEscape[T](code: RawEscape)
      extends SegmentRaw[T]
      with Phantom

  ///////////
  /// App ///
  ///////////
  sealed trait App[T]                                   extends Shape[T]
  final case class Prefix[T](func: T, off: Int, arg: T) extends App[T]
  final case class Infix[T](
    larg: T,
    loff: Int,
    opr: T,
    roff: Int,
    rarg: T
  ) extends App[T]

  sealed trait Section[T] extends App[T]
  final case class SectionLeft[T](arg: T, off: Int, opr: AST.Opr)
      extends Section[T]
  final case class SectionRight[T](opr: AST.Opr, off: Int, arg: T)
      extends Section[T]
  final case class SectionSides[T](opr: AST.Opr) extends Section[T] with Phantom

  // Note: [Custom Encoder]
  final case class Block[T](
    ty: Block.Type,
    indent: Int,
    emptyLines: List[Int],
    firstLine: Block.Line[T],
    lines: List[Block.Line[Option[T]]],
    isOrphan: Boolean = false
  ) extends Shape[T] {
    // FIXME: Compatibility mode
    def replaceType(ntyp: Block.Type): Block[T] = copy(ty = ntyp)
    def replaceFirstLine(line: Block.Line[T]): Block[T] =
      copy(firstLine = line)
    def replaceLines(lines: List[Block.Line[Option[T]]]): Block[T] =
      copy(lines = lines)
  }

  /* Note [Custom encoder]
   * ~~~~~~~~~~~~~~~~~~~~~
   * This type uses custom encoder in [[AstOps.toJson]]. It needs to be updated
   * as well, when fields are changed.
   */

  //////////////
  /// Module ///
  //////////////
  final case class Module[T](lines: List1[Block.OptLine[T]]) extends Shape[T] {
    def setLines(lines: List1[Block.OptLine[T]]) = copy(lines = lines)
  }

  /////////////
  /// Macro ///
  /////////////
  sealed trait Macro[T] extends Shape[T]
  final case class Match[T](
    pfx: Option[Pattern.Match],
    segs: Shifted.List1[Match.Segment[T]],
    resolved: Option[AST]
  ) extends Macro[T] {
    def path: List1[AST] = segs.toList1().map(_.wrapped.head)
  }
  final case class Ambiguous[T](
    segs: Shifted.List1[Ambiguous.Segment],
    paths: Tree[AST, Unit]
  ) extends Macro[T]

  /////////////////////
  /// Spaceless AST ///
  /////////////////////
  sealed trait SpacelessAST[T] extends Shape[T]
  final case class Comment[T](lines: List[String])
      extends SpacelessAST[T]
      with Phantom
  final case class Documented[T](doc: Doc, emptyLinesBetween: Int, ast: T)
      extends SpacelessAST[T]
  final case class Import[T](
    path: List1[AST.Ident.Cons],
    rename: Option[AST.Ident.Cons],
    isAll: Boolean,
    onlyNames: Option[List1[AST.Ident.Cons]],
    hidingNames: Option[List1[AST.Ident.Cons]]
  ) extends SpacelessAST[T]
  final case class Export[T](
    path: List1[AST.Ident.Cons],
    rename: Option[AST.Ident.Cons],
    isAll: Boolean,
    onlyNames: Option[List1[AST.Ident]],
    hidingNames: Option[List1[AST.Ident]]
  ) extends SpacelessAST[T]
  final case class JavaImport[T](
    path: List1[AST.Ident],
    rename: Option[AST.Ident.Cons]
  ) extends SpacelessAST[T]
  final case class Mixfix[T](name: List1[AST.Ident], args: List1[T])
      extends SpacelessAST[T]
  final case class Group[T](body: Option[T])          extends SpacelessAST[T]
  final case class SequenceLiteral[T](items: List[T]) extends SpacelessAST[T]
  final case class TypesetLiteral[T](expression: Option[T])
      extends SpacelessAST[T]
  final case class Def[T](name: AST.Cons, args: List[T], body: Option[T])
      extends SpacelessAST[T]
  final case class Foreign[T](indent: Int, lang: String, code: List[String])
      extends SpacelessAST[T]
  final case class Modified[T](modifier: String, definition: T)
      extends SpacelessAST[T]

  //////////////////////////////////////////////////////////////////////////////
  // Companion objects /////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  // TODO: All companion objects can be generated with macros

  /** Helper to gather common implementation for traits betwen [[Shape]] and
    * leaf case classes. They are implemented by delegating to [[Shape]] trait
    * implementation.
    */
  trait IntermediateTrait[S[U] <: Shape[U]] {
    implicit def repr[T: Repr]: Repr[S[T]] = Shape.repr[T].repr(_)
    //    implicit def ozip[T: HasSpan]: OffsetZip[S, T] = { ident =>
    //      Shape.ozip[T].zipWithOffset(ident).asInstanceOf
    //    //OffsetZip[Shape, T](ident).asInstanceOf
    //    }
    implicit def span[T: HasSpan]: HasSpan[S[T]] =
      t => (t: Shape[T]).span()
  }

  object Unrecognized {
    implicit def ftor: Functor[Unrecognized]         = semi.functor
    implicit def fold: Foldable[Unrecognized]        = semi.foldable
    implicit def repr[T]: Repr[Unrecognized[T]]      = _.str
    implicit def ozip[T]: OffsetZip[Unrecognized, T] = t => t.coerce
    implicit def span[T]: HasSpan[Unrecognized[T]]   = _.str.length
  }

  object Unexpected {
    implicit def ftor: Functor[Unexpected]          = semi.functor
    implicit def fold: Foldable[Unexpected]         = semi.foldable
    implicit def repr[T: Repr]: Repr[Unexpected[T]] = t => Repr(t.stream)
    implicit def ozip[T: HasSpan]: OffsetZip[Unexpected, T] =
      t => t.copy(stream = OffsetZip(t.stream))
    implicit def span[T: HasSpan]: HasSpan[Unexpected[T]] =
      t => t.stream.span()
  }
  object Ident {
    implicit def ftor: Functor[Ident]    = semi.functor
    implicit def fold: Foldable[Ident]   = semi.foldable
    implicit def repr[T]: Repr[Ident[T]] = _.name
    implicit def ozip[T: HasSpan]: OffsetZip[Ident, T] = { ident =>
      OffsetZip[Shape, T](ident).asInstanceOf
    }
  }
  object Blank {
    implicit def ftor: Functor[Blank]         = semi.functor
    implicit def fold: Foldable[Blank]        = semi.foldable
    implicit def repr[T]: Repr[Blank[T]]      = _.name
    implicit def ozip[T]: OffsetZip[Blank, T] = t => t.coerce
    implicit def span[T]: HasSpan[Blank[T]]   = _ => 1
  }
  object Var {
    implicit def ftor: Functor[Var]         = semi.functor
    implicit def fold: Foldable[Var]        = semi.foldable
    implicit def repr[T]: Repr[Var[T]]      = _.name
    implicit def ozip[T]: OffsetZip[Var, T] = t => t.coerce
    implicit def span[T]: HasSpan[Var[T]]   = t => t.name.length
  }
  object Cons {
    implicit def ftor: Functor[Cons]         = semi.functor
    implicit def fold: Foldable[Cons]        = semi.foldable
    implicit def repr[T]: Repr[Cons[T]]      = _.name
    implicit def ozip[T]: OffsetZip[Cons, T] = t => t.coerce
    implicit def span[T]: HasSpan[Cons[T]]   = t => t.name.length
  }
  object Mod {
    implicit def ftor: Functor[Mod]         = semi.functor
    implicit def fold: Foldable[Mod]        = semi.foldable
    implicit def repr[T]: Repr[Mod[T]]      = R + _.name + "="
    implicit def ozip[T]: OffsetZip[Mod, T] = t => t.coerce
    implicit def span[T]: HasSpan[Mod[T]]   = t => t.name.length + 1
  }
  object Opr {
    implicit def ftor: Functor[Opr]         = semi.functor
    implicit def fold: Foldable[Opr]        = semi.foldable
    implicit def repr[T]: Repr[Opr[T]]      = _.name
    implicit def ozip[T]: OffsetZip[Opr, T] = t => t.coerce
    implicit def span[T]: HasSpan[Opr[T]]   = t => t.name.length
  }
  object Annotation {
    implicit def ftor: Functor[Annotation]         = semi.functor
    implicit def fold: Foldable[Annotation]        = semi.foldable
    implicit def repr[T]: Repr[Annotation[T]]      = _.name
    implicit def ozip[T]: OffsetZip[Annotation, T] = t => t.coerce
    implicit def span[T]: HasSpan[Annotation[T]]   = t => t.name.length
  }
  object InvalidSuffix {
    implicit def ftor: Functor[InvalidSuffix]         = semi.functor
    implicit def fold: Foldable[InvalidSuffix]        = semi.foldable
    implicit def ozip[T]: OffsetZip[InvalidSuffix, T] = t => t.coerce
    implicit def repr[T]: Repr[InvalidSuffix[T]] =
      t => R + t.elem.repr + t.suffix
    implicit def span[T]: HasSpan[InvalidSuffix[T]] =
      t => t.elem.span() + t.suffix.length
  }
  object Literal extends IntermediateTrait[Literal] {
    implicit def ftor: Functor[Literal]  = semi.functor
    implicit def fold: Foldable[Literal] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Literal, T] = { t =>
      OffsetZip[Shape, T](t).asInstanceOf
    }
  }
  object Number {
    implicit def fromInt[T](int: Int): AST.Number = AST.Number(int)
    implicit def ftor: Functor[Number]            = semi.functor
    implicit def fold: Foldable[Number]           = semi.foldable
    implicit def ozip[T]: OffsetZip[Number, T]    = t => t.coerce
    implicit def repr[T]: Repr[Number[T]] =
      t => t.base.map(_ + "_").getOrElse("") + t.int
    implicit def span[T]: HasSpan[Number[T]] =
      t => t.base.map(_.length + 1).getOrElse(0) + t.int.length
  }
  object DanglingBase {
    implicit def ftor: Functor[DanglingBase]         = semi.functor
    implicit def fold: Foldable[DanglingBase]        = semi.foldable
    implicit def repr[T]: Repr[DanglingBase[T]]      = R + _.base + '_'
    implicit def ozip[T]: OffsetZip[DanglingBase, T] = t => t.coerce
    implicit def span[T]: HasSpan[DanglingBase[T]] =
      t => t.base.length + 1
  }
  object Text extends IntermediateTrait[Text] {
    implicit def ftor: Functor[Text]  = semi.functor
    implicit def fold: Foldable[Text] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Text, T] = { t =>
      OffsetZip[Shape, T](t).asInstanceOf
    }
  }
  object TextUnclosed {
    implicit def ftor: Functor[TextUnclosed]  = semi.functor
    implicit def fold: Foldable[TextUnclosed] = semi.foldable
    implicit def repr[T: Repr]: Repr[TextUnclosed[T]] = {
      case TextUnclosed(t: TextLineRaw[T]) => t.quote + t.text
      case TextUnclosed(t: TextLineFmt[T]) => t.quote + t.text
    }
    implicit def ozip[T: HasSpan]: OffsetZip[TextUnclosed, T] =
      t => t.copy(line = OffsetZip(t.line))
    implicit def span[T: HasSpan]: HasSpan[TextUnclosed[T]] = {
      case TextUnclosed(t: TextLineRaw[T]) => t.quote.span + t.text.span()
      case TextUnclosed(t: TextLineFmt[T]) => t.quote.span + t.text.span()
    }
  }
  object InvalidQuote {
    implicit def ftor: Functor[InvalidQuote]          = semi.functor
    implicit def fold: Foldable[InvalidQuote]         = semi.foldable
    implicit def repr[T: Repr]: Repr[InvalidQuote[T]] = _.quote
    implicit def ozip[T]: OffsetZip[InvalidQuote, T]  = t => t.coerce
    implicit def span[T]: HasSpan[InvalidQuote[T]]    = _.quote.span
  }
  object InlineBlock {
    implicit def ftor: Functor[InlineBlock]         = semi.functor
    implicit def fold: Foldable[InlineBlock]        = semi.foldable
    implicit def repr[T]: Repr[InlineBlock[T]]      = _.quote
    implicit def ozip[T]: OffsetZip[InlineBlock, T] = t => t.coerce
    implicit def span[T]: HasSpan[InlineBlock[T]]   = _.quote.span
  }
  object TextLine extends IntermediateTrait[TextLine] {
    implicit def ftor: Functor[TextLine]  = semi.functor
    implicit def fold: Foldable[TextLine] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[TextLine, T] = {
      case t: TextLineRaw[T] => OffsetZip(t)
      case t: TextLineFmt[T] => OffsetZip(t)
    }
  }
  object TextLineRaw {
    implicit def ftor: Functor[TextLineRaw]  = semi.functor
    implicit def fold: Foldable[TextLineRaw] = semi.foldable
    implicit def repr[T: Repr]: Repr[TextLineRaw[T]] =
      t => t.quote + t.text + t.quote
    implicit def ozip[T]: OffsetZip[TextLineRaw, T] = t => t.coerce
    implicit def span[T: HasSpan]: HasSpan[TextLineRaw[T]] =
      t => (2 * t.quote.span) + t.text.map(_.span()).sum
  }
  object TextLineFmt {
    implicit def ftor: Functor[TextLineFmt]  = semi.functor
    implicit def fold: Foldable[TextLineFmt] = semi.foldable
    implicit def repr[T: Repr]: Repr[TextLineFmt[T]] =
      t => t.quote + t.text + t.quote
    implicit def ozip[T: HasSpan]: OffsetZip[TextLineFmt, T] = { t =>
      var offset = Index(t.quote.span)
      val text2 = for (elem <- t.text) yield {
        val offElem = elem.map(offset -> _)
        offset += Size(elem.span())
        offElem
      }
      TextLineFmt(text2)
    }
    implicit def span[T: HasSpan]: HasSpan[TextLineFmt[T]] =
      t => (2 * t.quote.span) + t.text.map(_.span()).sum
  }

  object TextBlock extends IntermediateTrait[TextBlock] {
    def lineRepr[T: Repr](off: Int, l: TextBlockLine[SegmentFmt[T]]): Builder =
      R + l.empty_lines.map(newline + _) + newline + off + l.text
    def lineSpan[T: HasSpan](off: Int, l: TextBlockLine[SegmentFmt[T]]): Int = {
      val emptyLinesSpan = l.empty_lines.map(newline.span + _).sum
      emptyLinesSpan + newline.span + off + l.text.span()
    }

    implicit def ftor: Functor[TextBlock]  = semi.functor
    implicit def fold: Foldable[TextBlock] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[TextBlock, T] = {
      case body: TextBlockRaw[T] => OffsetZip(body)
      case body: TextBlockFmt[T] => OffsetZip(body)
    }
  }

  object TextBlockRaw {
    implicit def ftor: Functor[TextBlockRaw]  = semi.functor
    implicit def fold: Foldable[TextBlockRaw] = semi.foldable
    implicit def repr[T: Repr]: Repr[TextBlockRaw[T]] =
      t => t.quote + t.spaces + t.text.map(TextBlock.lineRepr(t.offset, _))
    implicit def ozip[T: HasSpan]: OffsetZip[TextBlockRaw, T] = t => t.coerce
    implicit def span[T: HasSpan]: HasSpan[TextBlockRaw[T]] = { t =>
      val linesSpan = t.text.map(TextBlock.lineSpan(t.offset, _)).sum
      t.quote.span + t.spaces + linesSpan
    }
  }

  object TextBlockFmt {
    implicit def ftor: Functor[TextBlockFmt]  = semi.functor
    implicit def fold: Foldable[TextBlockFmt] = semi.foldable
    implicit def repr[T: Repr]: Repr[TextBlockFmt[T]] =
      t => t.quote + t.spaces + t.text.map(TextBlock.lineRepr(t.offset, _))
    implicit def ozip[T: HasSpan]: OffsetZip[TextBlockFmt, T] = { body =>
      var offset = Index(body.quote.span)
      val text =
        for (line <- body.text) yield {
          offset += Size(line.empty_lines.length + line.empty_lines.sum)
          offset += Size(1 + body.offset)
          val text = for (elem <- line.text) yield {
            val offElem = elem.map(offset -> _)
            offset += Size(elem.span())
            offElem
          }
          line.copy(text = text)
        }
      body.copy(text = text)
    }
    implicit def span[T: HasSpan]: HasSpan[TextBlockFmt[T]] = { t =>
      val linesSpan = t.text.map(TextBlock.lineSpan(t.offset, _)).sum
      t.quote.span + t.spaces + linesSpan
    }
  }

  object Segment {
    implicit def ftor: Functor[Segment]  = semi.functor
    implicit def fold: Foldable[Segment] = semi.foldable
    implicit def repr[T: Repr]: Repr[Segment[T]] = {
      case t: SegmentRaw[T] => Repr(t)
      case t: SegmentFmt[T] => Repr(t)
    }
    implicit def ozip[T: HasSpan]: OffsetZip[Segment, T] = {
      case t: SegmentRaw[T] => OffsetZip(t)
      case t: SegmentFmt[T] => OffsetZip(t)
    }
    implicit def span[T: HasSpan]: HasSpan[Segment[T]] = {
      case t: SegmentRaw[T] => t.span()
      case t: SegmentFmt[T] => t.span()
    }
  }
  object SegmentFmt {
    implicit def ftor[T]: Functor[SegmentFmt] = semi.functor
    implicit def fold: Foldable[SegmentFmt]   = semi.foldable
    implicit def repr[T: Repr]: Repr[SegmentFmt[T]] = {
      case t: SegmentPlain[T]     => Repr(t)
      case t: SegmentExpr[T]      => Repr(t)
      case t: SegmentEscape[T]    => Repr(t)
      case t: SegmentRawEscape[T] => Repr(t)
    }
    implicit def ozip[T]: OffsetZip[SegmentFmt, T] = {
      case t: SegmentPlain[T]     => OffsetZip(t)
      case t: SegmentExpr[T]      => OffsetZip(t)
      case t: SegmentEscape[T]    => OffsetZip(t)
      case t: SegmentRawEscape[T] => OffsetZip(t)
    }
    implicit def span[T: HasSpan]: HasSpan[SegmentFmt[T]] = {
      case t: SegmentPlain[T]     => t.span()
      case t: SegmentExpr[T]      => t.span()
      case t: SegmentEscape[T]    => t.span()
      case t: SegmentRawEscape[T] => t.span()
    }
  }
  object SegmentRaw {
    implicit def ftor[T]: Functor[SegmentRaw] = semi.functor
    implicit def fold: Foldable[SegmentRaw]   = semi.foldable
    implicit def repr[T]: Repr[SegmentRaw[T]] = {
      case t: SegmentPlain[T]     => Repr(t)
      case t: SegmentRawEscape[T] => Repr(t)
    }
    implicit def ozip[T]: OffsetZip[SegmentRaw, T] = {
      case t: SegmentPlain[T]     => OffsetZip(t)
      case t: SegmentRawEscape[T] => OffsetZip(t)
    }
    implicit def span[T]: HasSpan[SegmentRaw[T]] = {
      case t: SegmentPlain[T]     => t.span()
      case t: SegmentRawEscape[T] => t.span()
    }
  }
  object SegmentPlain {
    implicit def txtFromString[T](str: String): SegmentPlain[T] =
      SegmentPlain(str)

    implicit def fold: Foldable[SegmentPlain]   = semi.foldable
    implicit def ftor[T]: Functor[SegmentPlain] = semi.functor
    implicit def repr[T]: Repr[SegmentPlain[T]] = _.value
    implicit def ozip[T]: OffsetZip[SegmentPlain, T] =
      t => t.coerce
    implicit def span[T]: HasSpan[SegmentPlain[T]] = _.value.length
  }
  object SegmentExpr {
    val quote: Repr.Builder = "`"

    implicit def ftor[T]: Functor[SegmentExpr] = semi.functor
    implicit def fold: Foldable[SegmentExpr]   = semi.foldable
    implicit def repr[T: Repr]: Repr[SegmentExpr[T]] =
      R + quote + _.value + quote
    implicit def ozip[T]: OffsetZip[SegmentExpr, T] =
      _.map(Index.Start -> _)
    implicit def span[T: HasSpan]: HasSpan[SegmentExpr[T]] =
      quote.span + _.value.span() + quote.span
  }
  object SegmentEscape {
    val introducer: Repr.Builder = "\\"

    implicit def ftor: Functor[SegmentEscape]  = semi.functor
    implicit def fold: Foldable[SegmentEscape] = semi.foldable
    implicit def repr[T]: Repr[SegmentEscape[T]] =
      t => introducer + t.code.repr
    implicit def ozip[T]: OffsetZip[SegmentEscape, T] =
      t => t.coerce
    implicit def span[T: HasSpan]: HasSpan[SegmentEscape[T]] =
      introducer.span + _.code.repr.length
  }
  object SegmentRawEscape {
    val introducer: Repr.Builder = "\\"

    implicit def ftor: Functor[SegmentRawEscape]  = semi.functor
    implicit def fold: Foldable[SegmentRawEscape] = semi.foldable
    implicit def repr[T]: Repr[SegmentRawEscape[T]] =
      t => introducer + t.code.repr
    implicit def ozip[T]: OffsetZip[SegmentRawEscape, T] =
      t => t.coerce
    implicit def span[T]: HasSpan[SegmentRawEscape[T]] =
      introducer.span + _.code.repr.length
  }
  object App extends IntermediateTrait[App] {
    implicit def ftor[T]: Functor[App] = semi.functor
    implicit def fold: Foldable[App]   = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[App, T] =
      t => OffsetZip[Shape, T](t).asInstanceOf
  }
  object Prefix {
    implicit def ftor: Functor[Prefix]  = semi.functor
    implicit def fold: Foldable[Prefix] = semi.foldable
    implicit def repr[T: Repr]: Repr[Prefix[T]] =
      t => R + t.func + t.off + t.arg
    implicit def ozip[T: HasSpan]: OffsetZip[Prefix, T] =
      t =>
        t.copy(
          func = (Index.Start, t.func),
          arg  = (Index(t.func.span() + t.off), t.arg)
        )
    implicit def span[T: HasSpan]: HasSpan[Prefix[T]] =
      t => t.func.span() + t.off + t.arg.span()

  }
  object Infix {
    implicit def ftor: Functor[Infix]  = semi.functor
    implicit def fold: Foldable[Infix] = semi.foldable
    implicit def repr[T: Repr]: Repr[Infix[T]] =
      t => R + t.larg + t.loff + t.opr + t.roff + t.rarg
    implicit def ozip[T: HasSpan]: OffsetZip[Infix, T] =
      t => {
        val larg = Index.Start                   -> t.larg
        val opr  = Index(t.larg.span() + t.loff) -> t.opr
        val rarg =
          Index(t.larg.span() + t.loff + t.opr.span() + t.roff) -> t.rarg
        t.copy(larg = larg, opr = opr, rarg = rarg)
      }
    implicit def span[T: HasSpan]: HasSpan[Infix[T]] =
      t => t.larg.span() + t.loff + t.opr.span() + t.roff + t.rarg.span()
  }

  object Section extends IntermediateTrait[Section] {
    implicit def ftor[T]: Functor[Section] = semi.functor
    implicit def fold: Foldable[Section]   = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Section, T] =
      t => OffsetZip[Shape, T](t).asInstanceOf
  }
  object SectionLeft {
    implicit def ftor: Functor[SectionLeft]  = semi.functor
    implicit def fold: Foldable[SectionLeft] = semi.foldable
    implicit def repr[T: Repr]: Repr[SectionLeft[T]] =
      t => R + t.arg + t.off + t.opr
    implicit def ozip[T]: OffsetZip[SectionLeft, T] =
      t => t.copy(arg = (Index.Start, t.arg))
    implicit def span[T: HasSpan]: HasSpan[SectionLeft[T]] =
      t => t.arg.span() + t.off + t.opr.span
  }
  object SectionRight {
    implicit def ftor: Functor[SectionRight]  = semi.functor
    implicit def fold: Foldable[SectionRight] = semi.foldable
    implicit def repr[T: Repr]: Repr[SectionRight[T]] =
      t => R + t.opr + t.off + t.arg
    implicit def ozip[T]: OffsetZip[SectionRight, T] =
      t => t.copy(arg = (Index(t.opr.span + t.off), t.arg))
    implicit def span[T: HasSpan]: HasSpan[SectionRight[T]] =
      t => t.opr.span + t.off + t.arg.span()
  }
  object SectionSides {
    implicit def ftor: Functor[SectionSides]          = semi.functor
    implicit def fold: Foldable[SectionSides]         = semi.foldable
    implicit def repr[T: Repr]: Repr[SectionSides[T]] = t => R + t.opr
    implicit def ozip[T]: OffsetZip[SectionSides, T]  = t => t.coerce
    implicit def span[T: HasSpan]: HasSpan[SectionSides[T]] =
      t => t.opr.span
  }

  object Block {
    implicit def ftorBlock: Functor[Block] = semi.functor
    implicit def fold: Foldable[Block]     = semi.foldable
    implicit def reprBlock[T: Repr]: Repr[Block[T]] =
      t => {
        val headRepr       = if (t.isOrphan) R else newline
        val emptyLinesRepr = t.emptyLines.map(R + _ + newline)
        val firstLineRepr  = R + t.indent + t.firstLine
        val linesRepr = t.lines.map { line =>
          newline + line.elem.map(_ => t.indent) + line
        }
        headRepr + emptyLinesRepr + firstLineRepr + linesRepr
      }
    implicit def ozipBlock[T: HasSpan]: OffsetZip[Block, T] =
      t => {
        var index = 0
        index += (if (t.isOrphan) 0 else 1)
        index += t.emptyLines.map(_ + 1).sum
        index += t.indent
        val line = t.firstLine.copy(elem = (Index(index), t.firstLine.elem))
        index += t.firstLine.span() + newline.span
        val lines = for (line <- t.lines) yield {
          val elem = line.elem.map(elem => {
            index += t.indent
            (Index(index), elem)
          })
          index += line.span() + newline.span
          line.copy(elem = elem)
        }
        t.copy(firstLine = line, lines = lines)
      }
    implicit def span[T: HasSpan]: HasSpan[Block[T]] =
      t => {
        val headSpan       = if (t.isOrphan) 0 else 1
        val emptyLinesSpan = t.emptyLines.map(_ + 1).sum
        val firstLineSpan  = t.indent + t.firstLine.span()
        def lineSpan(line: Shape.Block.OptLine[T]): Int = {
          val indentSpan = if (line.elem.isDefined) t.indent else 0
          newline.span + indentSpan + line.span()
        }
        val linesSpan = t.lines.map(lineSpan).sum
        headSpan + emptyLinesSpan + firstLineSpan + linesSpan
      }

    /// Block type ///
    sealed trait Type
    final case object Continuous    extends Type
    final case object Discontinuous extends Type

    /// Block Line ///
    type OptLine[T] = Line[Option[T]]
    final case class Line[+T](elem: T, off: Int) {
      // FIXME: Compatibility mode
      def toOptional: Line[Option[T]] = copy(elem = Some(elem))
    }
    object Line {
      implicit def ftor: Functor[Line]          = semi.functor
      implicit def fold: Foldable[Line]         = semi.foldable
      implicit def repr[T: Repr]: Repr[Line[T]] = t => R + t.elem + t.off
      implicit def span[T: HasSpan]: HasSpan[Line[T]] =
        t => t.elem.span() + t.off
      implicit def spanOpt[T: HasSpan]: HasSpan[OptLine[T]] =
        t => t.elem.map(_.span()).getOrElse(0) + t.off
    }
  }

  object Module {
    implicit def ftor: Functor[Module]  = semi.functor
    implicit def fold: Foldable[Module] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Module, T] =
      t => {
        var index = 0
        val lines = t.lines.map { line =>
          val elem = line.elem.map((Index(index), _))
          index += line.span() + newline.span
          line.copy(elem = elem)
        }
        t.copy(lines = lines)
      }

    implicit def repr[T: Repr]: Repr[Module[T]] =
      t => R + t.lines.head + t.lines.tail.map(newline + _)
    implicit def span[T: HasSpan]: HasSpan[Module[T]] =
      t => t.lines.span() + (t.lines.size - 1) * newline.span
  }

  object Macro extends IntermediateTrait[Macro] {
    implicit def ftor[T]: Functor[Macro] = semi.functor
    implicit def fold: Foldable[Macro]   = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Macro, T] =
      t => OffsetZip[Shape, T](t).asInstanceOf
  }

  object Match {
    /// Instances ///
    implicit def ftor: Functor[Match]  = semi.functor
    implicit def fold: Foldable[Match] = semi.foldable
    implicit def ozip[T: HasSpan]: OffsetZip[Match, T] =
      t => {
        var off = 0
        t.copy(segs = t.segs.map { seg =>
          OffsetZip(seg).map(_.map(_.map(s => {
            val loff = off
            off = s._2.span()
            (s._1 + Size(loff), s._2)
          })))
        })
      }
    @nowarn("cat=unused-imports")
    implicit def repr[T: Repr]: Repr[Match[T]] =
      t => {
        import AST.ASTOf._
        val pfxStream = t.pfx.map(_.toStream.reverse).getOrElse(List())
        val pfxRepr   = pfxStream.map(t => R + t.wrapped + t.off)
        R + pfxRepr + t.segs
      }
    implicit def span[T: HasSpan]: HasSpan[Match[T]] = { t =>
      val pfxSpan  = t.pfx.span()
      val segsSpan = t.segs.span()
      pfxSpan + segsSpan
    }

    /// Segment ///
    final case class Segment[T](
      head: AST.Ident,
      body: Pattern.MatchOf[Shifted[T]]
    ) {
      def isValid: Boolean = body.isValid
      def map(
        f: Pattern.MatchOf[Shifted[T]] => Pattern.MatchOf[Shifted[T]]
      ): Segment[T] =
        copy(body = f(body))
    }

    object Segment {
      implicit def repr[T: Repr]: Repr[Segment[T]] =
        t => R + t.head + t.body
      implicit def ozip[T: HasSpan]: OffsetZip[Segment, T] =
        t => {
          t.copy(body = OffsetZip(t.body).map { case (i, s) =>
            s.map((i + Size(t.head.span), _))
          })
        }
      implicit def span[T: HasSpan]: HasSpan[Segment[T]] =
        t => t.head.span + t.body.span()

      def apply[T](head: AST.Ident): Shape.Match.Segment[T] =
        Shape.Match.Segment(head, Pattern.Match.Nothing())
    }
  }

  object Ambiguous {
    implicit def ftor: Functor[Ambiguous]  = semi.functor
    implicit def fold: Foldable[Ambiguous] = semi.foldable
    implicit def repr[T]: Repr[Ambiguous[T]] =
      t => R + t.segs.map(Repr(_))
    implicit def ozip[T]: OffsetZip[Ambiguous, T] =
      _.map(Index.Start -> _)
    implicit def span[T: HasSpan]: HasSpan[Ambiguous[T]] = t => t.segs.span()

    final case class Segment(head: AST, body: Option[AST.SAST])
    object Segment {
      def apply(head: AST): Segment       = Segment(head, None)
      implicit def repr: Repr[Segment]    = t => R + t.head + t.body
      implicit def span: HasSpan[Segment] = t => t.head.span + t.body.span()
    }
  }

  object Comment {
    val symbol                           = "#"
    implicit def ftor: Functor[Comment]  = semi.functor
    implicit def fold: Foldable[Comment] = semi.foldable
    implicit def repr[T]: Repr[Comment[T]] =
      R + symbol + symbol + _.lines.mkString("\n")
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Comment, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Comment[T]]   = _ => 0
  }

  object Documented {
    import Comment.symbol
    implicit def ftor[T]: Functor[Documented]  = semi.functor
    implicit def fold[T]: Foldable[Documented] = semi.foldable
    implicit def repr[T: Repr]: Repr[Documented[T]] =
      t => {
        val symbolRepr = R + symbol + symbol
        val betweenDocAstRepr =
          R + newline + newline.build() * t.emptyLinesBetween
        R + symbolRepr + t.doc + betweenDocAstRepr + t.ast
      }
    implicit def offsetZip[T]: OffsetZip[Documented, T] =
      _.map(Index.Start -> _)
    implicit def span[T: HasSpan]: HasSpan[Documented[T]] = _ => 0

    implicit def toJson[T]: Encoder[Documented[T]] =
      _ => throw new NotImplementedError()
  }

  object Import {
    implicit def ftor: Functor[Import]  = semi.functor
    implicit def fold: Foldable[Import] = semi.foldable
    implicit def repr[T]: Repr[Import[T]] =
      t => R + "import" + t.path.repr.build()

    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Import, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Import[T]]   = _ => 0
  }

  object Export {
    implicit def ftor: Functor[Export]  = semi.functor
    implicit def fold: Foldable[Export] = semi.foldable
    implicit def repr[T]: Repr[Export[T]] =
      t => R + "export" + t.path.repr.build()

    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Export, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Export[T]]   = _ => 0
  }

  object JavaImport {
    implicit def ftor: Functor[JavaImport]  = semi.functor
    implicit def fold: Foldable[JavaImport] = semi.foldable
    implicit def repr[T]: Repr[JavaImport[T]] =
      t =>
        R + ("polyglot java import " + t.path
          .map(_.repr.build())
          .toList
          .mkString("."))

    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[JavaImport, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[JavaImport[T]]   = _ => 0
  }

  object Mixfix {
    implicit def ftor: Functor[Mixfix]  = semi.functor
    implicit def fold: Foldable[Mixfix] = semi.foldable
    implicit def repr[T: Repr]: Repr[Mixfix[T]] =
      t => {
        val lastRepr = if (t.name.length == t.args.length) List() else List(R)
        val argsRepr = t.args.toList.map(R + " " + _) ++ lastRepr
        val nameRepr = t.name.toList.map(Repr(_))
        R + nameRepr.lazyZip(argsRepr).map(_ + _)
      }
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Mixfix, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Mixfix[T]]   = _ => 0
  }

  object Group {
    implicit def ftor: Functor[Group]  = semi.functor
    implicit def fold: Foldable[Group] = semi.foldable
    implicit def repr[T: Repr]: Repr[Group[T]] =
      R + "(" + _.body + ")"
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Group, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Group[T]]   = _ => 0
  }

  object SequenceLiteral {
    implicit def ftor: Functor[SequenceLiteral]  = semi.functor
    implicit def fold: Foldable[SequenceLiteral] = semi.foldable
    implicit def repr[T: Repr]: Repr[SequenceLiteral[T]] =
      t => R + "[" + t.items.map(_.repr.build()).mkString(", ") + "]"
    implicit def ozip[T]: OffsetZip[SequenceLiteral, T] =
      _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[SequenceLiteral[T]] = _ => 0

  }

  object TypesetLiteral {
    implicit def ftor: Functor[TypesetLiteral] = semi.functor
    implicit def fold: Foldable[Def]           = semi.foldable
    implicit def repr[T: Repr]: Repr[TypesetLiteral[T]] =
      t => s"{ ${t.expression.repr.build()} }"
    implicit def ozip[T]: OffsetZip[TypesetLiteral, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[TypesetLiteral[T]]   = _ => 0
  }

  object Def {
    implicit def ftor: Functor[Def]  = semi.functor
    implicit def fold: Foldable[Def] = semi.foldable
    implicit def repr[T: Repr]: Repr[Def[T]] =
      t => R + Def.symbol + 1 + t.name + t.args.map(R + 1 + _) + t.body
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Def, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Def[T]]   = _ => 0

    val symbol = "def"
  }

  object Foreign {
    implicit def ftor: Functor[Foreign]  = semi.functor
    implicit def fold: Foldable[Foreign] = semi.foldable
    implicit def repr[T: Repr]: Repr[Foreign[T]] =
      t => {
        val code2 = t.code.map(R + t.indent + _).mkString("\n")
        R + "foreign " + t.lang + "\n" + code2
      }
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Foreign, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Foreign[T]]   = _ => 0
  }

  object Modified {
    implicit def ftor: Functor[Modified]  = semi.functor
    implicit def fold: Foldable[Modified] = semi.foldable
    implicit def repr[T: Repr]: Repr[Modified[T]] =
      t => {
        R + t.modifier + t.definition.repr.build()
      }
    // FIXME: How to make it automatic for non-spaced AST?
    implicit def ozip[T]: OffsetZip[Modified, T] = _.map(Index.Start -> _)
    implicit def span[T]: HasSpan[Modified[T]]   = _ => 0
  }

  //// Implicits ////

  object implicits {
    implicit class ToShapeOps[T[S] <: Shape[S]](t: T[AST])(implicit
      functor: Functor[T],
      ozip: OffsetZip[T, AST]
    ) {

      def map(f: AST => AST): T[AST] = {
        Functor[T].map(t)(f)
      }

      def mapWithOff(f: (Index, AST) => AST): T[AST] =
        Functor[T].map(ozip.zipWithOffset(t))(f.tupled)
    }

    implicit class ToShapeOpsRepr[T[S] <: Shape[S]](t: T[AST])(implicit
      repr: Repr[T[AST]]
    ) {
      def show(): String = repr.repr(t).build()
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//// Shape Boilerplate /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// TODO [MWU] The repr, ozip and span are almost entirely boilerplate.
//  Consider providing them using macros.
sealed trait ShapeImplicit {
  import Shape._

  implicit def ftor: Functor[Shape]  = semi.functor
  implicit def fold: Foldable[Shape] = semi.foldable
  implicit def repr[T: Repr]: Repr[Shape[T]] = {
    case s: Unrecognized[T]  => s.repr
    case s: Unexpected[T]    => s.repr
    case s: Blank[T]         => s.repr
    case s: Var[T]           => s.repr
    case s: Cons[T]          => s.repr
    case s: Opr[T]           => s.repr
    case s: Annotation[T]    => s.repr
    case s: Mod[T]           => s.repr
    case s: InvalidSuffix[T] => s.repr
    case s: Number[T]        => s.repr
    case s: DanglingBase[T]  => s.repr
    case s: TextUnclosed[T]  => s.repr
    case s: InvalidQuote[T]  => s.repr
    case s: InlineBlock[T]   => s.repr
    case s: TextLineRaw[T]   => s.repr
    case s: TextLineFmt[T]   => s.repr
    case s: TextBlockRaw[T]  => s.repr
    case s: TextBlockFmt[T]  => s.repr
    case s: Prefix[T]        => s.repr
    case s: Infix[T]         => s.repr
    case s: SectionLeft[T]   => s.repr
    case s: SectionRight[T]  => s.repr
    case s: SectionSides[T]  => s.repr
    case s: Block[T]         => s.repr
    case s: Module[T]        => s.repr
    case s: Ambiguous[T]     => s.repr
    case s: Match[T]         => s.repr
    // spaceless
    case s: Comment[T]    => s.repr
    case s: Documented[T] => s.repr
    case s: Import[T]     => s.repr
    case s: Export[T]     => s.repr

    case s: JavaImport[T]      => s.repr
    case s: Mixfix[T]          => s.repr
    case s: Group[T]           => s.repr
    case s: SequenceLiteral[T] => s.repr
    case s: TypesetLiteral[T]  => s.repr
    case s: Def[T]             => s.repr
    case s: Foreign[T]         => s.repr
    case s: Modified[T]        => s.repr
  }
  implicit def ozip[T: HasSpan]: OffsetZip[Shape, T] = {
    case s: Unrecognized[T]  => OffsetZip[Unrecognized, T].zipWithOffset(s)
    case s: Unexpected[T]    => OffsetZip[Unexpected, T].zipWithOffset(s)
    case s: Blank[T]         => OffsetZip[Blank, T].zipWithOffset(s)
    case s: Var[T]           => OffsetZip[Var, T].zipWithOffset(s)
    case s: Cons[T]          => OffsetZip[Cons, T].zipWithOffset(s)
    case s: Opr[T]           => OffsetZip[Opr, T].zipWithOffset(s)
    case s: Annotation[T]    => OffsetZip[Annotation, T].zipWithOffset(s)
    case s: Mod[T]           => OffsetZip[Mod, T].zipWithOffset(s)
    case s: InvalidSuffix[T] => OffsetZip[InvalidSuffix, T].zipWithOffset(s)
    case s: Number[T]        => OffsetZip[Number, T].zipWithOffset(s)
    case s: DanglingBase[T]  => OffsetZip[DanglingBase, T].zipWithOffset(s)
    case s: TextUnclosed[T]  => OffsetZip[TextUnclosed, T].zipWithOffset(s)
    case s: InvalidQuote[T]  => OffsetZip[InvalidQuote, T].zipWithOffset(s)
    case s: InlineBlock[T]   => OffsetZip[InlineBlock, T].zipWithOffset(s)
    case s: TextLineRaw[T]   => OffsetZip[TextLineRaw, T].zipWithOffset(s)
    case s: TextLineFmt[T]   => OffsetZip[TextLineFmt, T].zipWithOffset(s)
    case s: TextBlockRaw[T]  => OffsetZip[TextBlockRaw, T].zipWithOffset(s)
    case s: TextBlockFmt[T]  => OffsetZip[TextBlockFmt, T].zipWithOffset(s)
    case s: Prefix[T]        => OffsetZip[Prefix, T].zipWithOffset(s)
    case s: Infix[T]         => OffsetZip[Infix, T].zipWithOffset(s)
    case s: SectionLeft[T]   => OffsetZip[SectionLeft, T].zipWithOffset(s)
    case s: SectionRight[T]  => OffsetZip[SectionRight, T].zipWithOffset(s)
    case s: SectionSides[T]  => OffsetZip[SectionSides, T].zipWithOffset(s)
    case s: Block[T]         => OffsetZip[Block, T].zipWithOffset(s)
    case s: Module[T]        => OffsetZip[Module, T].zipWithOffset(s)
    case s: Ambiguous[T]     => OffsetZip[Ambiguous, T].zipWithOffset(s)
    case s: Match[T]         => OffsetZip[Match, T].zipWithOffset(s)
    // spaceless
    case s: Comment[T]    => OffsetZip[Comment, T].zipWithOffset(s)
    case s: Documented[T] => OffsetZip[Documented, T].zipWithOffset(s)
    case s: Import[T]     => OffsetZip[Import, T].zipWithOffset(s)
    case s: Export[T]     => OffsetZip[Export, T].zipWithOffset(s)

    case s: JavaImport[T]      => OffsetZip[JavaImport, T].zipWithOffset(s)
    case s: Mixfix[T]          => OffsetZip[Mixfix, T].zipWithOffset(s)
    case s: Group[T]           => OffsetZip[Group, T].zipWithOffset(s)
    case s: SequenceLiteral[T] => OffsetZip[SequenceLiteral, T].zipWithOffset(s)
    case s: TypesetLiteral[T]  => OffsetZip[TypesetLiteral, T].zipWithOffset(s)
    case s: Def[T]             => OffsetZip[Def, T].zipWithOffset(s)
    case s: Foreign[T]         => OffsetZip[Foreign, T].zipWithOffset(s)
    case s: Modified[T]        => OffsetZip[Modified, T].zipWithOffset(s)
  }

  implicit def span[T: HasSpan]: HasSpan[Shape[T]] = {
    case s: Unrecognized[T]  => s.span()
    case s: Unexpected[T]    => s.span()
    case s: Blank[T]         => s.span()
    case s: Var[T]           => s.span()
    case s: Cons[T]          => s.span()
    case s: Opr[T]           => s.span()
    case s: Annotation[T]    => s.span()
    case s: Mod[T]           => s.span()
    case s: InvalidSuffix[T] => s.span()
    case s: Number[T]        => s.span()
    case s: DanglingBase[T]  => s.span()
    case s: TextUnclosed[T]  => s.span()
    case s: InvalidQuote[T]  => s.span()
    case s: InlineBlock[T]   => s.span()
    case s: TextLineRaw[T]   => s.span()
    case s: TextLineFmt[T]   => s.span()
    case s: TextBlockRaw[T]  => s.span()
    case s: TextBlockFmt[T]  => s.span()
    case s: Prefix[T]        => s.span()
    case s: Infix[T]         => s.span()
    case s: SectionLeft[T]   => s.span()
    case s: SectionRight[T]  => s.span()
    case s: SectionSides[T]  => s.span()
    case s: Block[T]         => s.span()
    case s: Module[T]        => s.span()
    case s: Ambiguous[T]     => s.span()
    case s: Match[T]         => s.span()
    // spaceless
    case s: Comment[T]         => s.span()
    case s: Documented[T]      => s.span()
    case s: Import[T]          => s.span()
    case s: Export[T]          => s.span()
    case s: JavaImport[T]      => s.span()
    case s: Mixfix[T]          => s.span()
    case s: Group[T]           => s.span()
    case s: SequenceLiteral[T] => s.span()
    case s: TypesetLiteral[T]  => s.span()
    case s: Def[T]             => s.span()
    case s: Foreign[T]         => s.span()
    case s: Modified[T]        => s.span()
  }
}

//////////////////////////////////////////////////////////////////////////////
//// AST /////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/** =AST=
  *
  * AST is encoded as a simple recursion scheme. See the following links to
  * learn more about the concept:
  * - https://wiki.haskell.org/Catamorphisms
  * - https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms
  * - https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
  * - http://hackage.haskell.org/package/free-5.1.2/docs/Control-Comonad-Cofree.html
  * - https://www.47deg.com/blog/basic-recursion-schemes-in-scala/
  *
  * ==AST Shape==
  *
  * Every AST node like [[AST.Ident.Var]] or [[AST.App.Prefix]] defines a shape
  * of its subtree. Shapes extend [[Shape]], are parametrized with a child type,
  * and are defined within the [[Shape]] object. Shapes contain information
  * about names of children and spacing between them, for example, the
  * [[Shape.Prefix]] shape contains reference to function being its first child
  * ([[Shape.Prefix.func]]), spacing between the function and its argument
  * ([[Shape.Prefix.off]]), and the argument itself ([[Shape.Prefix.arg]]).
  *
  * ==[[ASTOf]] as Catamorphism==
  *
  * In order to keep the types simple and make the inference predictive, we
  * are not using standard catamorphism implementations. Instead, we have
  * implemented a simple recursion scheme in [[ASTOf]]. Every AST node uses it
  * as the wrapping layer. For example, the most generic AST type, [[AST]] is
  * defined just as an alias to [[(ASTOf[Shape])]]. Every AST node follows
  * the same scheme, including [[AST.Ident.Var]] being an alias to
  * [[(ASTOf[Shape.Var])]], or [[AST.App.Prefix]] being an alias to
  * [[(ASTOf[Shape.Prefix])]].
  *
  * ==[[ASTOf]] as Cofree==
  *
  * [[ASTOf]] adds a layer of additional information to each AST node.
  *
  * Currently the additional information include only an optional [[UUID]] and
  * cached span value, however this set might grow in the future. This design
  * minimizes the necessary boilerplate in storing repeatable information across
  * AST. Moreover, we can easily make [[ASTOf]] polymorphic and allow the Syntax
  * Tree to be tagged with different information in different compilation stages
  * if necessary.
  *
  * ==[[ASTOf]] as Cache Layer==
  *
  * Shapes and AST nodes implement several type classes, including:
  * - [[Functor]]   - Defines mapping over every element in a shape.
  * - [[Repr]]      - Defines shape to code translation.
  * - [[OffsetZip]] - Zips every shape element with offset from the left side
  *                   of the shape.
  *
  * [[ASTOf]] caches the span value. This way querying AST subtree for it span
  * is always O(1).
  *
  * ==[[ASTOf]] as Method Provider==
  *
  * Because [[ASTOf]] has access to all the type class instances of the child
  * element (and they cannot be further exposed because [[ASTOf]] type parameter
  * has to be variant), it is a perfect place for exposing common utils for AST
  * nodes. Please note, that "exposing" means both providing as well as caching.
  * For example, when we eval `myAST.map(a => a)` we are not doing pattern match
  * as one may expect. During the creation of [[ASTOf]], the functor of the
  * shape was obtained and the `map` method references it, so instead of pattern
  * matching, we are acessing the `map` method directly.
  *
  * ==Fields Access==
  *
  * Please note, that [[ASTOf]] is "transparent". There are
  * implicit defs of both wrapping and unwrapping functions, which makes
  * working with AST nodes very convenient. For example, there is no need to
  * write `myVar.shape.name` to first unpack the node from the [[ASTOf]] layer
  * and then access its name. It's possible to just write `myVar.name`, and
  * the unpacking will be performed automatically.
  *
  * ==Pattern Matching==
  *
  * Please note that due to type erasure, it is impossible to pattern match on
  * AST types. Never use `case _: Var => ...` statement, as it will probably
  * crash at runtime. In order to pattern match on AST type, each AST node
  * provides a special "any" matcher, which results in a type narrowed version
  * of the AST node. For example, `case Var.any(v) => ...` will succeed if the
  * match was performed on any [[AST.Ident.Var]] and its result `v` will be of
  * [[AST.Ident.Var]] type. Of course, it is possible to use structural matching
  * without any restrictions.
  */
object AST {
  import Shape.implicits._

  //////////////////////////////////////////////////////////////////////////////
  //// Reexports ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Assoc = opr.Assoc
  val Assoc = opr.Assoc
  val Prec  = opr.Prec

  //////////////////////////////////////////////////////////////////////////////
  //// Definition //////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Structure ////

  //  type Shape = Shape[AST]
  type _AST = ASTOf[Shape]

  //// Aliases ////

  type SAST         = Shifted[AST]
  type StreamOf[T]  = List[Shifted[T]]
  type StreamOf1[T] = List1[Shifted[T]]
  type Stream       = StreamOf[AST]
  type Stream1      = StreamOf1[AST]
  type ID           = UUID

  //// API ////

  def tokenize(ast: AST): Shifted.List1[AST] = {
    @tailrec
    def go(ast: AST, out: AST.Stream): Shifted.List1[AST] =
      ast match {
        case App.Prefix.any(t) => go(t.func, Shifted(t.off, t.arg) :: out)
        case _                 => Shifted.List1(ast, out)
      }
    go(ast, List())
  }

  //// Conversions ////

  object conversions extends conversions
  sealed trait conversions extends Ident.conversions {
    implicit def intToAST(int: Int): AST =
      Literal.Number(int)

    implicit def stringToAST(str: String): AST = {
      if (str == "") throw new Error("Empty literal")
      if (str == "_") Blank()
      else if (str.head.isLower) Var(str)
      else if (str.head.isUpper) Cons(str)
      else Opr(str)
    }
  }

  ////////////////////////////////////
  //// Apply / Unapply Generators ////
  ////////////////////////////////////

  /** [[Unapply]] and [[UnapplyByType]] are unapply generators for AST Shapes.
    * The implementation may seem complex, but this is just a scala way for
    * deconstructing types. When provided with a AST type, like [[Ident.Var]],
    * [[Unapply]] deconstructs it to [[(ASTOf[VarOf])]] and then generates
    * an object providing unapply implementation for the [[VarOf]] type.
    */
  sealed trait Unapply[T] {
    type In
    def run[Out](f: In => Out)(t: AST): Option[Out]
  }
  object Unapply {
    def apply[T](implicit t: Unapply[T]): Unapply[T] { type In = t.In } = t
    implicit def inst[T[_]](implicit
      ev: ClassTag[T[AST]]
    ): Unapply[ASTOf[T]] { type In = T[AST] } =
      new Unapply[ASTOf[T]] {
        type In = T[AST]
        val ct                              = implicitly[ClassTag[T[AST]]]
        def run[Out](fn: In => Out)(t: AST) = ct.unapply(t.shape).map(fn)
      }
  }

  /** See the documentation for [[Unapply]] */
  sealed trait UnapplyByType[T] {
    def unapply(t: AST): Option[T]
  }
  object UnapplyByType {
    def apply[T](implicit ev: UnapplyByType[T]) = ev
    implicit def instance[T[_]](implicit
      ct: ClassTag[T[_]]
    ): UnapplyByType[ASTOf[T]] =
      new UnapplyByType[ASTOf[T]] {
        def unapply(t: AST) =
          // Note that the `asInstanceOf` usage is safe here.
          // It is used only for performance reasons, otherwise we would need
          // to create a new object which would look exactly the same way
          // as the original one.
          ct.unapply(t.shape).map(_ => t.asInstanceOf[ASTOf[T]])
      }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// ASTOf ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Definition ////

  /** The [[ASTOf]] class wraps each AST node. The implementation is similar to
    * standard catamorphic Fix, however, it is parametrized with the head shape
    * type. In combination with covariance of [[T]], it allows us to both keep
    * information about the specific shape of the AST, as well as get natural
    * subtyping behavior. For example, [[AST]] and [[Var]] are aliases to
    * [[(ASTOf[Shape])]] and [[(AST[VarOf])]] respectively, and while
    * [[(VarOf[T] <: Shape[T])]], also [[Var <: AST]].
    *
    * Another important role of [[ASTOf]] is caching of [[Repr.Builder]] and
    * allowing for fast method redirection. When [[ASTOf]] is created, it
    * remembers a bunch of stuff, which can be fast accessed even if we cast the
    * type to generic [[AST]].
    */
  final case class ASTOf[+T[_]](
    shape: T[AST],
    span: Int,
    id: Option[ID]             = None,
    location: Option[Location] = None
  ) {
    override def toString        = s"Node($id,$location,$shape)"
    override def hashCode(): Int = shape.hashCode()

    def setID(newID: Option[ID]): ASTOf[T] = copy(id = newID)
    def setID(newID: ID): ASTOf[T]         = setID(Some(newID))
    def withNewID(): ASTOf[T]              = copy(id = Some(UUID.randomUUID()))
    def withNewIDIfMissing(): ASTOf[T] =
      id match {
        case Some(_) => this
        case None    => this.withNewID()
      }
    def setLocation(newLocation: Option[Location]): ASTOf[T] =
      copy(location = newLocation)
    def setLocation(newLocation: Location): ASTOf[T] =
      setLocation(Some(newLocation))

    /** Compares ignoring cached span value and node ID.
      *
      * Cached span values become invalid e.g. after macros are resolved.
      * Also, they are not really a part data information, they are more like
      * intermediary cache used by some components that are aware of its
      * caveats.
      *
      * ID also does not should be treated as part of node contents, it is used
      * e.g. to map external data onto AST.
      */
    override def equals(rhs: Any): Boolean = {
      rhs match {
        case rhs: ASTOf[_] =>
          this.shape == rhs.shape
        case _ => false
      }
    }
  }

  object ASTOf extends AstImplicits

  trait AstImplicits extends AstImplicits2 {
    implicit def unwrap[T[_]](t: ASTOf[T]): T[AST] = t.shape
    implicit def repr[T[S] <: Shape[S]]: Repr[ASTOf[T]] =
      t => implicitly[Repr[Shape[AST]]].repr(t.shape)
    implicit def span[T[_]]: HasSpan[ASTOf[T]] = t => t.span
    implicit def wrap[T[_]](
      t: T[AST]
    )(implicit ev: HasSpan[T[AST]]): ASTOf[T] = ASTOf(t, ev.span(t))

    implicit def encoder_spec(implicit
      ev: Encoder[Shape[AST]]
    ): Encoder[AST] = encoder
  }

  trait AstImplicits2 {
    // Note: [JSON Schema]
    implicit def encoder[T[S] <: Shape[S]](implicit
      ev: Encoder[Shape[AST]]
    ): Encoder[ASTOf[T]] =
      ast => {
        import io.circe.syntax._

        val shape  = "shape" -> ev(ast.shape)
        val id     = ast.id.map("id" -> _.asJson)
        val span   = "span"  -> ast.span.asJson
        val fields = Seq(shape) ++ id.toSeq :+ span
        Json.fromFields(fields)
      }
  }

  /* Note: [JSON Schema]
   * ~~~~~~~~~~~~~~~~~~~
   * Each AST node is serialized to a map with `shape`, `span` and,
   * optionally, `id` keys. `shape` is always serialized as if base trait
   * were encoded, even if the final case class type is known. This is
   * required for consistency with Rust AST implementation, which uses a
   * monomorphic AST type and has no means of expressing types like
   * `AstOf[Shape.Var]`.
   */

  //// ASTOps ////

  /** [[ASTOps]] implements handy AST operations.
    *
    * Implementations in this class do not require any special knowledge of the
    * underlying shape and thus are just a high-level AST addons.
    */
  implicit class AstOps[T[S] <: Shape[S]](t: ASTOf[T])(implicit
    functor: Functor[T],
    fold: Foldable[T],
    repr: Repr[T[AST]],
    ozip: OffsetZip[T, AST]
  ) {
    def show(): String = repr.repr(t.shape).build()

    def map(f: AST => AST): ASTOf[T] =
      t.copy(shape = t.shape.map(f))

    def foldMap[A](f: AST => A)(implicit A: Monoid[A]): A =
      fold.foldMap(t.shape)(f)

    def mapWithOff(f: (Index, AST) => AST): ASTOf[T] =
      t.copy(shape = ToShapeOps(t.shape).mapWithOff(f))

    def traverseWithOff(f: (Index, AST) => AST): ASTOf[T] = {
      def go(i: Index, ast: AST): AST =
        ast.mapWithOff((j, ast) => go(i + j.asSize, f(i + j.asSize, ast)))
      t.mapWithOff((j, ast) => go(j, f(j, ast)))
    }

    def zipWithOffset(): T[(Index, AST)] = {
      OffsetZip(t.shape)
    }

    def idMap(implicit ev: Foldable[Shape]): List[(Span, AST.ID)] = {
      var ids  = List[(Span, AST.ID)]()
      var asts = List[(Index, AST)](Index.Start -> t)
      while (asts.nonEmpty) {
        val (off, ast) = asts.head
        val children = ast.zipWithOffset().toList.map { case (o, ast) =>
          (o + off.asSize, ast)
        }
        if (ast.id.nonEmpty)
          ids +:= Span(off, ast) -> ast.id.get
        asts = children ++ asts.tail
      }
      ids.reverse
    }

    // Note [JSON Serialization]
    def toJson(): Json = {
      import io.circe.generic.auto._
      import io.circe.syntax._

      // Note [JSON Format Customizations]
      @nowarn("msg=parameter value evidence")
      @unused implicit def blockEncoder[A: Encoder]: Encoder[Shape.Block[A]] =
        block =>
          Json.obj(
            "ty"          -> block.ty.asJson,
            "indent"      -> block.indent.asJson,
            "empty_lines" -> block.emptyLines.asJson,
            "first_line"  -> block.firstLine.asJson,
            "lines"       -> block.lines.asJson,
            "is_orphan"   -> block.isOrphan.asJson
          )

      // Note (below) [JSON Format Customizations]
      @unused implicit def escapeEncoder: Encoder[Escape] = {
        case e: Escape.Character =>
          Json.obj("Character" -> Json.obj("c" -> e.repr.asJson))
        case e: Escape.Control =>
          val fields =
            Json.obj("name" -> e.repr.asJson, "code" -> e.code.asJson)
          Json.obj("Control" -> fields)
        case e: Escape.Number =>
          Json.obj("Number" -> Json.obj("digits" -> e.repr.asJson))
        case e: Escape.Unicode.U16 =>
          Json.obj("Unicode16" -> Json.obj("digits" -> e.digits.asJson))
        case e: Escape.Unicode.U21 =>
          Json.obj("Unicode21" -> Json.obj("digits" -> e.digits.asJson))
        case e: Escape.Unicode.U32 =>
          Json.obj("Unicode32" -> Json.obj("digits" -> e.digits.asJson))
        case _ => throw new RuntimeException("Cannot encode escape.")
      }

      val ast: AST = t
      ast.asJson
    }
  }

  /* Note [JSON Format Customizations]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * The JSON generated from serializing AST with `toJson` method is meant to be
   * consumed by Rust. To ensure compatibility, several adjustments are made:
   *  * custom encoding for Escape, as Rust currently does not have the same
   *    hierarchy of types for that AST nodes.
   *  * custom encoding of fields which are not compatible with snake_case
   *    style.
   */

  //////////////////////////////////////////////////////////////////////////////
  //// Invalid /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Invalid = ASTOf[Shape.Invalid]
  object Invalid {
    type Unrecognized = AST.ASTOf[Shape.Unrecognized]
    type Unexpected   = AST.ASTOf[Shape.Unexpected]

    val any = UnapplyByType[Invalid]

    object Unrecognized {
      val any = UnapplyByType[Unrecognized]
      def unapply(t: AST) =
        Unapply[Unrecognized].run(_.str)(t)
      def apply(str: String): Unrecognized = Shape.Unrecognized[AST](str)
    }
    object Unexpected {
      val any = UnapplyByType[Unexpected]
      def unapply(t: AST) =
        Unapply[Unexpected].run(t => (t.msg, t.stream))(t)
      def apply(msg: String, str: Stream): Unexpected =
        Shape.Unexpected(msg, str)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Ident ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Reexports ////

  type Blank      = Ident.Blank
  type Var        = Ident.Var
  type Cons       = Ident.Cons
  type Opr        = Ident.Opr
  type Mod        = Ident.Mod
  type Annotation = Ident.Annotation

  val Blank      = Ident.Blank
  val Var        = Ident.Var
  val Cons       = Ident.Cons
  val Opr        = Ident.Opr
  val Mod        = Ident.Mod
  val Annotation = Ident.Annotation

  //// Definition ////

  type Ident = ASTOf[Shape.Ident]

  object Ident {
    type Blank      = ASTOf[Shape.Blank]
    type Var        = ASTOf[Shape.Var]
    type Cons       = ASTOf[Shape.Cons]
    type Mod        = ASTOf[Shape.Mod]
    type Opr        = ASTOf[Shape.Opr]
    type Annotation = ASTOf[Shape.Annotation]

    type InvalidSuffix = ASTOf[Shape.InvalidSuffix]

    object InvalidSuffix {
      val any = UnapplyByType[InvalidSuffix]
      def unapply(t: AST) =
        Unapply[InvalidSuffix].run(t => (t.elem, t.suffix))(t)
      def apply(elem: Ident, suffix: String): InvalidSuffix =
        Shape.InvalidSuffix[AST](elem, suffix)
    }

    //// Conversions ////

    trait Conversions1 {
      implicit def strToVar(str: String): Var   = Var(str)
      implicit def strToCons(str: String): Cons = Cons(str)
      implicit def strToOpr(str: String): Opr   = Opr(str)
      implicit def strToMod(str: String): Mod   = Mod(str)
    }

    trait conversions extends Conversions1 {
      implicit def stringToIdent(str: String): Ident = {
        if (str == "") throw new Error("Empty literal")
        if (str == "_") Blank()
        else if (str.head.isLower) Var(str)
        else if (str.head.isUpper) Cons(str)
        else Opr(str)
      }
    }

    //// Smart Constructors ////

    val any = UnapplyByType[Ident]

    object Blank {
      private val cachedBlank = Shape.Blank[AST]()
      val any                 = UnapplyByType[Blank]
      def unapply(t: AST)     = Unapply[Blank].run(_ => true)(t)
      def apply(): Blank      = cachedBlank
    }
    object Var {
      val any                      = UnapplyByType[Var]
      def unapply(t: AST)          = Unapply[Var].run(_.name)(t)
      def apply(name: String): Var = Shape.Var[AST](name)
    }
    object Cons {
      val any                       = UnapplyByType[Cons]
      def unapply(t: AST)           = Unapply[Cons].run(_.name)(t)
      def apply(name: String): Cons = Shape.Cons[AST](name)
    }
    object Mod {
      val any                      = UnapplyByType[Mod]
      def unapply(t: AST)          = Unapply[Mod].run(_.name)(t)
      def apply(name: String): Mod = Shape.Mod[AST](name)
    }
    object Opr {
      val app                      = Opr(" ")
      val any                      = UnapplyByType[Opr]
      def unapply(t: AST)          = Unapply[Opr].run(_.name)(t)
      def apply(name: String): Opr = Shape.Opr[AST](name)
    }
    object Annotation {
      val any                             = UnapplyByType[Annotation]
      def unapply(t: AST)                 = Unapply[Annotation].run(_.name)(t)
      def apply(name: String): Annotation = Shape.Annotation[AST](name)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Literal /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Reexports ////

  type Number = Literal.Number
  type Text   = Literal.Text
  val Number = Literal.Number
  val Text   = Literal.Text

  //// Definition ////

  type Literal = ASTOf[Shape.Literal]
  object Literal {

    val any = UnapplyByType[Literal]

    ////////////////
    //// Number ////
    ////////////////

    type Number = ASTOf[Shape.Number]
    object Number {
      type DanglingBase = ASTOf[Shape.DanglingBase]

      //// Smart Constructors ////
      def apply(i: String): Number            = Number(None, i)
      def apply(b: String, i: String): Number = Number(Some(b), i)
      def apply(i: Int): Number               = Number(i.toString)
      def apply(b: Int, i: String): Number    = Number(b.toString, i)
      def apply(b: String, i: Int): Number    = Number(b, i.toString)
      def apply(b: Int, i: Int): Number       = Number(b.toString, i.toString)
      def apply(b: Option[String], i: String): Number =
        Shape.Number[AST](b, i)
      def unapply(t: AST) = Unapply[Number].run(t => (t.base, t.int))(t)
      val any             = UnapplyByType[Number]

      //// DanglingBase ////
      object DanglingBase {
        val any                               = UnapplyByType[DanglingBase]
        def apply(base: String): DanglingBase = Shape.DanglingBase[AST](base)
        def unapply(t: AST) =
          Unapply[DanglingBase].run(_.base)(t)
      }
    }

    //////////////
    //// Text ////
    //////////////

    type Text = ASTOf[Shape.Text]
    object Text {
      val any = UnapplyByType[Text]

      //// Definition ////

      object Line {
        val Raw = Shape.TextLineRaw
        type Raw[T] = Shape.TextLineRaw[T]
        val Fmt = Shape.TextLineFmt
        type Fmt[T] = Shape.TextLineFmt[T]
      }

      object Block {
        val Line = Shape.TextBlockLine
        type Line[T] = Shape.TextBlockLine[T]
        val Raw = Shape.TextBlockRaw
        type Raw[T] = Shape.TextBlockRaw[T]
        val Fmt = Shape.TextBlockFmt
        type Fmt[T] = Shape.TextBlockFmt[T]
      }

      ////// CONSTRUCTORS ///////
      type Unclosed = ASTOf[Shape.TextUnclosed]
      object Unclosed {
        val any = UnapplyByType[Unclosed]
        def unapply(t: AST) =
          Unapply[Unclosed].run(t => t.line)(t)
        def apply(segment: Segment.Fmt*): Unclosed =
          Shape.TextUnclosed(Line.Fmt(segment.toList))
        object Raw {
          def apply(segment: Segment.Raw*): Unclosed =
            Shape.TextUnclosed(Line.Raw(segment.toList))
        }
      }
      type InvalidQuote = ASTOf[Shape.InvalidQuote]
      object InvalidQuote {
        val any = UnapplyByType[InvalidQuote]
        def unapply(t: AST) =
          Unapply[InvalidQuote].run(t => t.quote)(t)
        def apply(quote: String): InvalidQuote = Shape.InvalidQuote[AST](quote)
      }
      type InlineBlock = ASTOf[Shape.InlineBlock]
      object InlineBlock {
        val any = UnapplyByType[InlineBlock]
        def unapply(t: AST) =
          Unapply[InlineBlock].run(t => t.quote)(t)
        def apply(quote: String): InlineBlock = Shape.InlineBlock[AST](quote)
      }

      def apply(text: Shape.Text[AST]): Text = text
      def apply(segment: Segment.Fmt*): Text = Text(Line.Fmt(segment.toList))
      def apply(
        spaces: Int,
        off: Int,
        line: Shape.TextBlockLine[Segment.Fmt]*
      ): Text =
        Text(Shape.TextBlockFmt(line.toList, spaces, off))

      object Raw {
        def apply(segment: Segment.Raw*): Text =
          Text(Line.Raw(segment.toList))
        def apply(
          spaces: Int,
          off: Int,
          line: Shape.TextBlockLine[Segment.Raw]*
        ): Text =
          Text(Shape.TextBlockRaw(line.toList, spaces, off))
      }

      /////////////////
      //// Segment ////
      /////////////////
      type Segment[T] = Shape.Segment[T]
      object Segment {

        val Escape = org.enso.syntax.text.ast.text.Escape
        type Escape    = org.enso.syntax.text.ast.text.Escape
        type RawEscape = org.enso.syntax.text.ast.text.RawEscape

        //// Definition ////

        val EscT = Shape.SegmentEscape
        type EscT = Shape.SegmentEscape[AST]
        val RawEscT = Shape.SegmentRawEscape
        type RawEscT = Shape.SegmentRawEscape[AST]
        val Fmt = Shape.SegmentFmt
        type Fmt = Shape.SegmentFmt[AST]
        val Raw = Shape.SegmentRaw
        type Raw = Shape.SegmentRaw[AST]

        object Esc {
          def apply(code: Escape): EscT = Shape.SegmentEscape(code)
          def unapply(shape: EscT): Option[Escape] =
            Shape.SegmentEscape.unapply(shape)
        }
        object RawEsc {
          def apply(code: RawEscape): RawEscT = Shape.SegmentRawEscape(code)
          def unapply(shape: RawEscT): Option[RawEscape] =
            Shape.SegmentRawEscape.unapply(shape)
        }
        object Expr {
          def apply(t: Option[AST]): Fmt = Shape.SegmentExpr(t)
          def unapply(shape: Shape.SegmentExpr[AST]): Option[Option[AST]] =
            Shape.SegmentExpr.unapply(shape)
        }
        object Plain {
          def apply(s: String): Raw = Shape.SegmentPlain(s)
          def unapply(shape: Shape.SegmentPlain[AST]): Option[String] =
            Shape.SegmentPlain.unapply(shape)
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// App /////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Definition ////

  type App = ASTOf[Shape.App]
  object App {
    val any = UnapplyByType[App]

    //// Constructors ////

    type Prefix = ASTOf[Shape.Prefix]
    type Infix  = ASTOf[Shape.Infix]

    //// Smart Constructors ////

    object Prefix {
      val any             = UnapplyByType[Prefix]
      def unapply(t: AST) = Unapply[Prefix].run(t => (t.func, t.arg))(t)
      def apply(fn: AST, off: Int, arg: AST): Prefix =
        Shape.Prefix(fn, off, arg)
      def apply(fn: AST, arg: AST): Prefix = Prefix(fn, 1, arg)
    }

    object Infix {
      val any             = UnapplyByType[Infix]
      def unapply(t: AST) = Unapply[Infix].run(t => (t.larg, t.opr, t.rarg))(t)
      def apply(larg: AST, loff: Int, opr: Opr, roff: Int, rarg: AST): Infix =
        Shape.Infix(larg, loff, opr, roff, rarg)
      def apply(larg: AST, loff: Int, opr: Opr, rarg: AST): Infix =
        Infix(larg, loff, opr, 1, rarg)
      def apply(larg: AST, opr: Opr, roff: Int, rarg: AST): Infix =
        Infix(larg, 1, opr, roff, rarg)
      def apply(larg: AST, opr: Opr, rarg: AST): Infix =
        Infix(larg, 1, opr, 1, rarg)
    }

    /////////////////
    //// Section ////
    /////////////////

    //// Reexports ////

    type Left  = Section.Left
    type Right = Section.Right
    type Sides = Section.Sides

    val Left  = Section.Left
    val Right = Section.Right
    val Sides = Section.Sides

    //// Definition ////

    type Section = ASTOf[Shape.Section]
    object Section {

      val any = UnapplyByType[Section]

      //// Constructors ////

      type Left  = ASTOf[Shape.SectionLeft]
      type Right = ASTOf[Shape.SectionRight]
      type Sides = ASTOf[Shape.SectionSides]

      //// Smart Constructors ////

      object Left {
        val any             = UnapplyByType[Left]
        def unapply(t: AST) = Unapply[Left].run(t => (t.arg, t.opr))(t)

        def apply(arg: AST, off: Int, opr: Opr): Left =
          Shape.SectionLeft(arg, off, opr)
        def apply(arg: AST, opr: Opr): Left = Left(arg, 1, opr)
      }
      object Right {
        val any             = UnapplyByType[Right]
        def unapply(t: AST) = Unapply[Right].run(t => (t.opr, t.arg))(t)

        def apply(opr: Opr, off: Int, arg: AST): Right =
          Shape.SectionRight(opr, off, arg)
        def apply(opr: Opr, arg: AST): Right = Right(opr, 1, arg)
      }
      object Sides {
        val any                    = UnapplyByType[Sides]
        def unapply(t: AST)        = Unapply[Sides].run(_.opr)(t)
        def apply(opr: Opr): Sides = Shape.SectionSides[AST](opr)
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Block ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Block = ASTOf[Shape.Block]

  object Block {
    type Type = Shape.Block.Type
    val Continuous    = Shape.Block.Continuous
    val Discontinuous = Shape.Block.Discontinuous

    //// Smart Constructors ////

    // FIXME: Compatibility mode
    def apply(
      @unused isOrphan: Boolean,
      typ: Type,
      indent: Int,
      emptyLines: List[Int],
      firstLine: Line,
      lines: List[OptLine]
    ): Block = {
      Shape.Block(typ, indent, emptyLines, firstLine, lines, isOrphan)
    }

    def apply(
      typ: Type,
      indent: Int,
      emptyLines: List[Int],
      firstLine: Line,
      lines: List[OptLine]
    ): Block = Shape.Block(typ, indent, emptyLines, firstLine, lines)

    def apply(
      indent: Int,
      firstLine: AST,
      lines: AST*
    ): Block =
      Block(
        Continuous,
        indent,
        List(),
        Line(firstLine),
        lines.toList.map(ast => Line(Some(ast)))
      )

    val any = UnapplyByType[Block]
    def unapply(t: AST) =
      Unapply[Block].run(t => (t.ty, t.indent, t.firstLine, t.lines))(t)

    //// Line ////

    type Line    = Shape.Block.Line[AST]
    type OptLine = Shape.Block.OptLine[AST]
    object Line {
      // FIXME: Compatibility mode
      type NonEmpty = Line
      val Required                    = Line
      def apply[T](elem: T, off: Int) = Shape.Block.Line(elem, off)
      def apply[T](elem: T)           = Shape.Block.Line(elem, 0)
    }
    object OptLine {
      def apply(): OptLine          = Line(None, 0)
      def apply(elem: AST): OptLine = Line(Some(elem))
      def apply(off: Int): OptLine  = Line(None, off)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Module //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Module = ASTOf[Shape.Module]

  object Module {
    import Block._
    type M = Module
    val any                                     = UnapplyByType[M]
    def unapply(t: AST)                         = Unapply[M].run(_.lines)(t)
    def apply(ls: List1[OptLine]): M            = Shape.Module(ls)
    def apply(l: OptLine): M                    = Module(List1(l))
    def apply(l: OptLine, ls: OptLine*): M      = Module(List1(l, ls.toList))
    def apply(l: OptLine, ls: List[OptLine]): M = Module(List1(l, ls))
    def traverseWithOff(m: M)(f: (Index, AST) => AST): M = {
      val lines2 = m.lines.map { line: OptLine =>
        // FIXME: Why line.map does not work?
        Shape.Block.Line.ftor.map(line)(_.map(_.traverseWithOff(f)))
      }
      m.shape.copy(lines = lines2)
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  //// Macro ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Macro = ASTOf[Shape.Macro]
  object Macro {

    //// Matched ////

    type Match = ASTOf[Shape.Match]

    object Match {
      val any = UnapplyByType[Match]
      def apply(
        pfx: Option[Pattern.Match],
        segs: Shifted.List1[Match.Segment],
        resolved: Option[AST]
      ): Match = Shape.Match[AST](pfx, segs, resolved)

      type Segment = Shape.Match.Segment[AST]
      val Segment = Shape.Match.Segment
      implicit class SegmentOps(t: Segment) {
        def toStream: AST.Stream = Shifted(t.head) :: t.body.toStream
      }

    }

    //// Ambiguous ////

    type Ambiguous = ASTOf[Shape.Ambiguous]
    object Ambiguous {
      type Segment = Shape.Ambiguous.Segment
      val Segment = Shape.Ambiguous.Segment
      def apply(
        segs: Shifted.List1[Segment],
        paths: Tree[AST, Unit]
      ): Ambiguous = ASTOf.wrap(Shape.Ambiguous(segs, paths))

      def unapply(t: AST) =
        Unapply[Ambiguous].run(t => (t.segs, t.paths))(t)
    }

    //// Resolver ////

    type Resolver = Resolver.Context => AST
    object Resolver {
      type Context = ContextOf[AST]
      final case class ContextOf[T](
        prefix: Option[Pattern.Match],
        body: List[Shape.Match.Segment[T]],
        id: ID
      )
      object Context {
        def apply(
          prefix: Option[Pattern.Match],
          body: List[Macro.Match.Segment],
          id: ID
        ): Context = ContextOf(prefix, body, id)
      }
    }

    //// Definition ////

    type Definition = __Definition__
    final case class __Definition__(
      back: Option[Pattern],
      init: List[Definition.Segment],
      last: Definition.LastSegment,
      resolver: Resolver
    ) {
      def path: List1[AST] = init.map(_.head) +: List1(last.head)
      def fwdPats: List1[Pattern] =
        init.map(_.pattern) +: List1(last.pattern.getOrElse(Pattern.Nothing()))
    }
    object Definition {
      import Pattern._

      final case class Segment(head: AST, pattern: Pattern) {
        def map(f: Pattern => Pattern): Segment = copy(pattern = f(pattern))
      }
      object Segment {
        type Tup = (AST, Pattern)
        def apply(t: Tup): Segment = Segment(t._1, t._2)
      }

      final case class LastSegment(head: AST, pattern: Option[Pattern]) {
        def map(f: Pattern => Pattern): LastSegment =
          copy(pattern = pattern.map(f))
      }
      object LastSegment {
        type Tup = (AST, Option[Pattern])
        def apply(t: Tup): LastSegment = LastSegment(t._1, t._2)
      }

      def apply(
        precSection: Option[Pattern],
        t1: Segment.Tup,
        ts: List[Segment.Tup]
      )(
        fin: Resolver
      ): Definition = {
        val segs    = List1(t1, ts)
        val init    = segs.init
        val lastTup = segs.last
        val last    = (lastTup._1, Some(lastTup._2))
        Definition(precSection, init, last, fin)
      }

      def apply(
        precSection: Option[Pattern],
        t1: Segment.Tup,
        ts: Segment.Tup*
      )(
        fin: Resolver
      ): Definition = Definition(precSection, t1, ts.toList)(fin)

      def apply(t1: Segment.Tup, t2_ : Segment.Tup*)(
        fin: Resolver
      ): Definition = Definition(None, t1, t2_.toList)(fin)

      def apply(initTups: List[Segment.Tup], lastHead: AST)(
        fin: Resolver
      ): Definition =
        Definition(None, initTups, (lastHead, None), fin)

      def apply(t1: Segment.Tup, last: AST)(fin: Resolver): Definition =
        Definition(List(t1), last)(fin)

      def apply(
        back: Option[Pattern],
        initTups: List[Segment.Tup],
        lastTup: LastSegment.Tup,
        resolver: Resolver
      ): Definition = {
        type PP = Pattern => Pattern
        val applyValidChecker: PP     = _ | ErrTillEnd("unmatched pattern")
        val applyFullChecker: PP      = _ :: ErrUnmatched("unmatched tokens")
        val applyDummyFullChecker: PP = _ :: Nothing()

        val unapplyValidChecker: Pattern.Match => Pattern.Match = {
          case Pattern.Match.Or(_, Left(tgt)) => tgt
          case _ =>
            throw new Error("Internal error")
        }
        val unapplyFullChecker: Pattern.Match => Pattern.Match = {
          case Pattern.Match.Seq(_, (tgt, _)) => tgt
          case _ =>
            throw new Error("Internal error")
        }
        val applySegInitCheckers: List[Segment] => List[Segment] =
          _.map(_.map(p => applyFullChecker(applyValidChecker(p))))

        val applySegLastCheckers: LastSegment => LastSegment =
          _.map(p => applyDummyFullChecker(applyValidChecker(p)))

        val unapplySegCheckers
          : List[AST.Macro.Match.Segment] => List[AST.Macro.Match.Segment] =
          _.map(_.map({
            case m @ Pattern.Match.Nothing(_) => m
            case m =>
              unapplyValidChecker(unapplyFullChecker(m))
          }))

        val initSegs           = initTups.map(Segment(_))
        val lastSeg            = LastSegment(lastTup)
        val backPatWithCheck   = back.map(applyValidChecker)
        val initSegsWithChecks = applySegInitCheckers(initSegs)
        val lastSegWithChecks  = applySegLastCheckers(lastSeg)

        def unexpected(ctx: Resolver.Context, msg: String): AST = {
          import AST.Macro.Match.SegmentOps
          val pfxStream  = ctx.prefix.map(_.toStream).getOrElse(List())
          val segsStream = ctx.body.flatMap(_.toStream)
          val stream     = pfxStream ++ segsStream
          AST.Invalid.Unexpected(msg, stream)
        }

        def resolverWithChecks(ctx: Resolver.Context) = {
          val pfxFail  = !ctx.prefix.forall(_.isValid)
          val segsFail = !ctx.body.forall(_.isValid)
          if (pfxFail || segsFail) unexpected(ctx, "invalid statement")
          else {
            try {
              val ctx2 = ctx.copy(
                prefix = ctx.prefix.map(unapplyValidChecker),
                body   = unapplySegCheckers(ctx.body)
              )
              resolver(ctx2)
            } catch {
              case _: Throwable =>
                val tokens = ctx.body.flatMap(mat => {
                  val firstElem = Shifted(mat.head)
                  val rest      = mat.body.toStream
                  firstElem :: rest
                })
                AST.Invalid.Unexpected("Unexpected tokens", tokens)
            }
          }
        }
        __Definition__(
          backPatWithCheck,
          initSegsWithChecks,
          lastSegWithChecks,
          resolverWithChecks
        )
      }

    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  //// Space-Unaware AST ///////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait SpacelessASTOf[T] extends Shape[T]

  //////////////////////////////////////////////////////////////////////////////
  /// Comment //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Comment = ASTOf[Shape.Comment]
  object Comment {
    val any = UnapplyByType[Comment]
    def apply(lines: List[String]): Comment =
      Shape.Comment(lines): Shape.Comment[AST]
    def unapply(t: AST): Option[List[String]] =
      Unapply[Comment].run(t => t.lines)(t)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Documented //////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Documented = ASTOf[Shape.Documented]

  object Documented {
    val any = UnapplyByType[Documented]
    def apply(doc: Doc, emp: Int, ast: AST): Documented =
      Shape.Documented(doc, emp, ast)
    def unapply(t: AST): Option[(Doc, Int, AST)] =
      Unapply[Documented].run(t => (t.doc, t.emptyLinesBetween, t.ast))(t)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Import //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Import = ASTOf[Shape.Import]

  object Import {
    def apply(
      path: List1[AST.Ident.Cons],
      rename: Option[AST.Ident.Cons],
      isAll: Boolean,
      onlyNames: Option[List1[AST.Ident.Cons]],
      hidingNames: Option[List1[AST.Ident.Cons]]
    ): Import =
      Shape.Import[AST](path, rename, isAll, onlyNames, hidingNames)
    def unapply(t: AST): Option[
      (
        List1[AST.Ident.Cons],
        Option[AST.Ident.Cons],
        Boolean,
        Option[List1[AST.Ident.Cons]],
        Option[List1[AST.Ident.Cons]]
      )
    ] =
      Unapply[Import].run(t =>
        (t.path, t.rename, t.isAll, t.onlyNames, t.hidingNames)
      )(t)
    val any = UnapplyByType[Import]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Export //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Export = ASTOf[Shape.Export]

  object Export {
    def apply(
      path: List1[AST.Ident.Cons],
      rename: Option[AST.Ident.Cons],
      isAll: Boolean,
      onlyNames: Option[List1[AST.Ident]],
      hidingNames: Option[List1[AST.Ident]]
    ): Export =
      Shape.Export[AST](path, rename, isAll, onlyNames, hidingNames)
    def unapply(t: AST): Option[
      (
        List1[AST.Ident.Cons],
        Option[AST.Ident.Cons],
        Boolean,
        Option[List1[AST.Ident]],
        Option[List1[AST.Ident]]
      )
    ] =
      Unapply[Export].run(t =>
        (t.path, t.rename, t.isAll, t.onlyNames, t.hidingNames)
      )(t)
    val any = UnapplyByType[Export]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Java Import /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type JavaImport = ASTOf[Shape.JavaImport]

  object JavaImport {
    def apply(path: List1[Ident], rename: Option[Ident.Cons]): JavaImport =
      Shape.JavaImport[AST](path, rename)
    def unapply(t: AST): Option[List1[Ident]] =
      Unapply[JavaImport].run(t => t.path)(t)
    val any = UnapplyByType[JavaImport]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Mixfix //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Mixfix = ASTOf[Shape.Mixfix]

  object Mixfix {
    def apply(name: List1[Ident], args: List1[AST]): Mixfix =
      Shape.Mixfix(name, args)
    def unapply(t: AST) = Unapply[Mixfix].run(t => (t.name, t.args))(t)
    val any             = UnapplyByType[Mixfix]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Group ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Group = ASTOf[Shape.Group]
  object Group {
    val any                                  = UnapplyByType[Group]
    def unapply(t: AST): Option[Option[AST]] = Unapply[Group].run(_.body)(t)
    def apply(body: Option[AST]): Group      = Shape.Group(body)
    def apply(body: AST): Group              = Group(Some(body))
    def apply(body: SAST): Group             = Group(body.wrapped)
    def apply(): Group                       = Group(None)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// SequenceLiteral /////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type SequenceLiteral = ASTOf[Shape.SequenceLiteral]
  object SequenceLiteral {
    val any = UnapplyByType[SequenceLiteral]
    def unapply(t: AST): Option[List[AST]] =
      Unapply[SequenceLiteral].run(_.items)(t)
    def apply(items: List[AST]): SequenceLiteral = Shape.SequenceLiteral(items)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Typeset Literal /////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type TypesetLiteral = ASTOf[Shape.TypesetLiteral]
  object TypesetLiteral {
    val any = UnapplyByType[TypesetLiteral]
    def unapply(t: AST): Option[Option[AST]] =
      Unapply[TypesetLiteral].run(_.expression)(t)
    def apply(expression: Option[AST]): TypesetLiteral =
      Shape.TypesetLiteral(expression)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Def /////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Def = ASTOf[Shape.Def]
  object Def {
    val any                                     = UnapplyByType[Def]
    def apply(name: Cons): Def                  = Def(name, List())
    def apply(name: Cons, args: List[AST]): Def = Def(name, args, None)
    def apply(name: Cons, args: List[AST], body: Option[AST]): Def =
      Shape.Def(name, args, body)
    def unapply(t: AST): Option[(Cons, List[AST], Option[AST])] =
      Unapply[Def].run(t => (t.name, t.args, t.body))(t)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Foreign /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Foreign = ASTOf[Shape.Foreign]
  object Foreign {
    def apply(indent: Int, lang: String, code: List[String]): Foreign =
      Shape.Foreign(indent, lang, code): Shape.Foreign[AST]
    def unapply(t: AST): Option[(Int, String, List[String])] =
      Unapply[Foreign].run(t => (t.indent, t.lang, t.code))(t)
    val any = UnapplyByType[Foreign]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Modified ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Modified = ASTOf[Shape.Modified]
  object Modified {
    def apply(modifier: String, definition: AST): Modified = {
      Shape.Modified(modifier, definition)
    }
    def unapply(t: AST): Option[(String, AST)] = {
      Unapply[Modified].run(t => (t.modifier, t.definition))(t)
    }
    val any = UnapplyByType[Modified]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Main ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  def main(): Unit = {
    val v1 = Ident.Var("foo")
    //    val v1_ = v1: AST

    println(v1.span)
    println((v1: AST.Ident).span)
    println(v1.span)

    //    println(v1_.asJson)
    //    val opr1 = Ident.Opr("+")
    val v2 = App.Prefix(Ident.Var("x"), 10, Ident.Var("z"))
    //    val v2_ = v2: AST
    //
    //    println(v2.asJson)
    //    println(v2_.asJson)
    println(OffsetZip(v2.shape))
  }
}

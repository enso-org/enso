package org.enso.syntax.text.ast.meta

import org.enso.data
import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Ident
import org.enso.syntax.text.AST.Macro
import Pattern.streamShift
import cats.data.NonEmptyList

import scala.annotation.tailrec

/////////////////
//// Builder ////
/////////////////

final class Builder(
  head: Ident,
  offset: Int                  = 0,
  lineBegin: Boolean           = false,
  val isModuleBuilder: Boolean = false
) {
  var context: Builder.Context           = Builder.Context()
  var macroDef: Option[Macro.Definition] = None
  var current: Builder.Segment           = new Builder.Segment(head, offset, lineBegin)
  var revSegs: List[Builder.Segment]     = List()

  def beginSegment(ast: Ident, off: Int): Unit = {
    revSegs ::= current
    current        = new Builder.Segment(ast)
    current.offset = off
  }

  def merge(that: Builder): Unit = {
    val revLeftStream = current.revStream
    val (revUnusedLeftTgt, matched, rightUnusedTgt) =
      that.build(revLeftStream)
    val result = List1(matched: AST.SAST, rightUnusedTgt)
    current.revStream = result.toList.reverse ++ revUnusedLeftTgt
  }

  def build(
    revStreamL: AST.Stream
  ): (AST.Stream, Shifted[Macro], AST.Stream) = {
    val revSegBldrs = List1(current, revSegs)
    macroDef match {
      case None =>
        val revSegs = revSegBldrs.map { segBldr =>
          val optAst = segBldr.buildAST()
          val seg    = Macro.Ambiguous.Segment(segBldr.ast, optAst)
          Shifted(segBldr.offset, seg)
        }
        val segments = revSegs.reverse
        val head     = segments.head
        val tail     = segments.tail
        val paths    = context.tree.dropValues()
        val stream   = Shifted.List1(head.wrapped, tail)
        val template = Macro.Ambiguous(stream, paths)
        val newTok   = Shifted(head.off, template)
        (revStreamL, newTok, List())

      case Some(mdef) =>
        val revSegPats    = mdef.fwdPats.reverse
        val revSegsOuts   = zipWith(revSegBldrs, revSegPats)(_.build(_))
        val revSegs       = revSegsOuts.map(_._1)
        val revSegStreams = revSegsOuts.map(_._2)
        val tailStream    = revSegStreams.head
        val segs          = revSegs.reverse

        val (segs2, pfxMatch, newLeftStream) = mdef.back match {
          case None => (segs, None, revStreamL)
          case Some(pat) =>
            val fstSegOff                = segs.head.off
            val (revStreamL2, lastLOff)  = streamShift(fstSegOff, revStreamL)
            val pfxMatch                 = pat.matchRevUnsafe(revStreamL2)
            val revStreamL3              = pfxMatch.stream
            val streamL3                 = revStreamL3.reverse
            val (streamL4, newFstSegOff) = streamShift(lastLOff, streamL3)
            val revStreamL4              = streamL4.reverse
            val newFirstSeg              = segs.head.copy(off = newFstSegOff)
            val newSegs                  = segs.copy(head = newFirstSeg)
            (newSegs, Some(pfxMatch.elem), revStreamL4)

        }

        val shiftSegs = Shifted.List1(segs2.head.wrapped, segs2.tail)

        if (!revSegStreams.tail.forall(_.isEmpty)) {
          throw new Error(
            "Internal error: not all template segments were fully matched"
          )
        }

        //        val resolved = mdef.fin(pfxMatch, shiftSegs.toList().map(_.el))
        val template = Macro.Match(pfxMatch, shiftSegs, null)
        val newTok   = Shifted(segs2.head.off, template)

        (newLeftStream, newTok, tailStream)

    }
  }

  // FIXME This is here because of bug in scalajs https://github.com/scala-js/scala-js/issues/3885
  private def zipWith[A, B, C](a: NonEmptyList[A], b: NonEmptyList[B])(
    f: (A, B) => C
  ): NonEmptyList[C] = {

    @tailrec
    def zwRev(as: List[A], bs: List[B], acc: List[C]): List[C] =
      (as, bs) match {
        case (Nil, Nil)         => acc // without this we get match error
        case (Nil, _)           => acc
        case (_, Nil)           => acc
        case (x :: xs, y :: ys) => zwRev(xs, ys, f(x, y) :: acc)
      }

    NonEmptyList(f(a.head, b.head), zwRev(a.tail, b.tail, Nil).reverse)
  }

  if (isModuleBuilder)
    macroDef = Some(
      Macro.Definition((AST.Blank(): AST) -> Pattern.Expr()) { ctx =>
        ctx.body match {
          case List(seg) =>
            seg.body.toStream match {
              case List(mod) => mod.wrapped
              case _         => throw new scala.Error("Impossible happened")
            }
        }
      }
    )

  def buildAsModule(): AST = {
    build(List())._2.wrapped match {
      case Macro.Match.any(m) =>
        m.segs.head.body.toStream match {
          case s :: Nil => s.wrapped
          case _        => throw new scala.Error("Impossible happened.")
        }
      case _ => throw new scala.Error("Impossible happened.")
    }
  }
}

object Builder {
  def moduleBuilder(): Builder =
    new Builder(AST.Blank(), isModuleBuilder = true, lineBegin = true)

  /////////////////
  //// Context ////
  /////////////////

  case class Context(tree: Registry.Tree, parent: Option[Context]) {
    def lookup(t: AST): Option[Registry.Tree] = tree.get(t)
    def isEmpty: Boolean                      = tree.isLeaf

    @tailrec
    final def parentLookup(t: AST): Boolean = {
      parent match {
        case None => false
        case Some(p) =>
          p.lookup(t) match {
            case None    => p.parentLookup(t)
            case Some(_) => true
          }
      }
    }
  }
  object Context {
    def apply(): Context                    = Context(data.Tree(), None)
    def apply(tree: Registry.Tree): Context = Context(tree, None)
  }

  /////////////////
  //// Segment ////
  /////////////////

  class Segment(
    val ast: Ident,
    var offset: Int        = 0,
    val lineBegin: Boolean = false
  ) {
    import Macro._
    var revStream: AST.Stream = List()

    def buildAST(): Option[Shifted[AST]] =
      Pattern.buildASTFrom(revStream.reverse)

    def build(
      pat: Pattern,
      reversed: Boolean = false
    ): (Shifted[Match.Segment], AST.Stream) = {
      val stream = revStream.reverse
      pat.matchOpt(stream, lineBegin, reversed) match {
        case None =>
          throw new Error(
            s"Internal error: template pattern segment was unmatched"
          )
        case Some(rr) =>
          (Shifted(offset, Match.Segment(ast, rr.elem)), rr.stream)
      }
    }

    //////////////////////////////////////

    override def toString: String =
      s"SegmentBuilder($offset, $revStream)"
  }
}

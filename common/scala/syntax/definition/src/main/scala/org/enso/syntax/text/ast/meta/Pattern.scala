package org.enso.syntax.text.ast.meta

import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.SAST
import org.enso.syntax.text.prec.Operator

import scala.annotation.tailrec
import org.enso.data.Index
import org.enso.data.Shifted
import org.enso.syntax.text.ast.Repr

////////////////////////////////////////////////////////////////////////////////
//// Pattern ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

object Pattern {
  import cats.Foldable
  import cats.Functor
  import cats.Traverse
  import cats.derived._

  type P      = Pattern
  type Spaced = Option[Boolean] // TODO [AA] Make this an actual ADT

  // TODO: Refactorme
  def streamShift_(off: Int, revStream: AST.Stream): AST.Stream =
    streamShift(off, revStream)._1

  def streamShift(off: Int, revStream: AST.Stream): (AST.Stream, Int) = {
    @tailrec
    def go(off: Int, str: AST.Stream, out: AST.Stream): (AST.Stream, Int) =
      str match {
        case Nil     => (out, off)
        case t :: ts => go(t.off, ts, Shifted(off, t.el) :: out)
      }
    val (nStream, nOff) = go(off, revStream, List())
    (nStream.reverse, nOff)
  }

  sealed trait Class
  object Class {
    final case object Normal  extends Class
    final case object Pattern extends Class
  }

  //// Primitive Constructors ////

  // format: off
  /** Boundary Patterns */
  final case class Begin   ()                                       extends P
  final case class End     ()                                       extends P

  /** Structural Patterns */
  final case class Nothing ()                                       extends P
  final case class Seq     (pat1 : P     , pat2   : P)              extends P
  final case class Or      (pat1 : P     , pat2   : P)              extends P
  final case class Many    (pat  : P)                               extends P
  final case class Except  (not  : P     , pat    : P)              extends P
  
  /** Meta Patterns */
  final case class Build   (pat  : P)                               extends P
  final case class Err     (msg  : String, pat    : P)              extends P
  final case class Tag     (tag  : String, pat    : P)              extends P
  final case class Cls     (cls  : Class , pat    : P)              extends P

  /** Token Patterns */
  final case class Tok     (spaced : Spaced, ast  : AST)            extends P
  final case class Blank   (spaced : Spaced)                        extends P
  final case class Var     (spaced : Spaced)                        extends P
  final case class Cons    (spaced : Spaced)                        extends P
  final case class Opr     (spaced : Spaced, maxPrec: Option[Int])  extends P
  final case class Mod     (spaced : Spaced)                        extends P
  final case class Num     (spaced : Spaced)                        extends P
  final case class Text    (spaced : Spaced)                        extends P
  final case class Block   (spaced : Spaced)                        extends P
  final case class Macro   (spaced : Spaced)                        extends P
  final case class Invalid (spaced : Spaced)                        extends P
  // format: on

  //// Smart Constructors ////

  object Tok {
    def apply(ast: AST): Tok = Tok(None, ast)
  }
  object Var {
    def apply():                Var = Var(None)
    def apply(spaced: Boolean): Var = Var(Some(spaced))
  }
  object Cons {
    def apply():                Cons = Cons(None)
    def apply(spaced: Boolean): Cons = Cons(Some(spaced))
  }
  object Opr {
    def apply():                Opr = Opr(None, None)
    def apply(spaced: Spaced):  Opr = Opr(spaced, None)
    def apply(spaced: Boolean): Opr = Opr(Some(spaced))
  }
  object Num {
    def apply():                Num = Num(None)
    def apply(spaced: Boolean): Num = Num(Some(spaced))
  }
  object Text {
    def apply():                Text = Text(None)
    def apply(spaced: Boolean): Text = Text(Some(spaced))
  }
  object Block {
    def apply():                Block = Block(None)
    def apply(spaced: Boolean): Block = Block(Some(spaced))
  }

  def Any(spaced: Spaced = None): Pattern =
    Blank(spaced) |
    Var(spaced) |
    Cons(spaced) |
    Opr(spaced) |
    Mod(spaced) |
    Num(spaced) |
    Text(spaced) |
    Block(spaced) |
    Macro(spaced) |
    Invalid(spaced)
  def Any(spaced: Boolean): Pattern = Any(Some(spaced))
  def ErrTillEnd(msg: String)   = Any().tillEnd.err(msg)
  def ErrUnmatched(msg: String) = End() | ErrTillEnd(msg)
  def Expr()                    = Any().many1.build
  def NonSpacedExpr()           = Any(spaced = false).many1.build
  def NonSpacedExpr_()          = (Any().but(Block()) :: Any(spaced = false).many).build
  def SepList(pat: Pattern, div: Pattern): Pattern = pat :: (div :: pat).many
  def SepList(pat: Pattern, div: Pattern, err: String): Pattern = {
    val seg = pat | Any().till(div).err(err)
    SepList(seg, div)
  }

  def ExprUntilOpr(opr: String) = {
    val base = Except(Opr(None, Some(AST.Opr(opr).prec)), Any())
    base.many1.build
  }

  //// Utils ////

  def buildASTFrom(stream: AST.Stream): Option[Shifted[AST]] =
    Operator.rebuild(stream)

  //// Conversions ////

  implicit def fromAST(ast: AST): Pattern = Tok(ast)

  //////////////////////////////////////////////////////////////////////////////
  //// Pattern.Match ///////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  object Match {
    type Switch[T] = Either[T, T]
    type M[T]      = MatchOf[T]
    type P         = Pattern
    val P = Pattern
    val A = AST

    //// Primitive Constructors ////

    // format: off
    /** Boundary Matches */
    final case class Begin   [T](pat:P.Begin)                       extends M[T]
    final case class End     [T](pat:P.End)                         extends M[T]

    /** Structural Matches */
    final case class Nothing [T](pat:P.Nothing)                     extends M[T]
    final case class Seq     [T](pat:P.Seq   , elem:(M[T], M[T]))   extends M[T]
    final case class Or      [T](pat:P.Or    , elem:Switch[M[T]])   extends M[T]
    final case class Many    [T](pat:P.Many  , elem:List[M[T]])     extends M[T]
    final case class Except  [T](pat:P.Except, elem:M[T])           extends M[T]

    /** Meta Matches */
    final case class Build [T](pat:P.Build , elem:T)                extends M[T]
    final case class Err   [T](pat:P.Err   , elem:T)                extends M[T]
    final case class Tag   [T](pat:P.Tag   , elem:M[T])             extends M[T]
    final case class Cls   [T](pat:P.Cls   , elem:M[T])             extends M[T]

    /** Token Matches */
    final case class Tok     [T](pat:P.Tok     , elem:T)            extends M[T]
    final case class Blank   [T](pat:P.Blank   , elem:T)            extends M[T]
    final case class Var     [T](pat:P.Var     , elem:T)            extends M[T]
    final case class Cons    [T](pat:P.Cons    , elem:T)            extends M[T]
    final case class Opr     [T](pat:P.Opr     , elem:T)            extends M[T]
    final case class Mod     [T](pat:P.Mod     , elem:T)            extends M[T]
    final case class Num     [T](pat:P.Num     , elem:T)            extends M[T]
    final case class Text    [T](pat:P.Text    , elem:T)            extends M[T]
    final case class Block   [T](pat:P.Block   , elem:T)            extends M[T]
    final case class Macro   [T](pat:P.Macro   , elem:T)            extends M[T]
    final case class Invalid [T](pat:P.Invalid , elem:T)            extends M[T]
    // format: on

    //// Smart Constructors ////

    object Nothing {
      def apply[T](): Match.Nothing[T] = Match.Nothing(Pattern.Nothing())
    }

    //// Result ////

    final case class Result(elem: Match, stream: AST.Stream) {
      def map(fn: Match => Match): Result = copy(elem = fn(elem))
    }

  }

  type Match = MatchOf[SAST]
  sealed trait MatchOf[T] {
    import MatchOf._
    import cats.implicits._

    val M = Match
    val pat: Pattern

    override def toString = s"Pattern.Match(${this.toStream})"

    def toStream: List[T] = this.map(List(_)).fold

    def mapStruct(f: MatchOf[T] => MatchOf[T]): MatchOf[T] =
      f(this.mapStructShallow(_.mapStruct(f)))

    def mapStructShallow(f: MatchOf[T] => MatchOf[T]): MatchOf[T] =
      this match {
        case m: M.Begin[T]   => m
        case m: M.End[T]     => m
        case m: M.Nothing[T] => m
        case m: M.Seq[T]     => m.copy(elem = m.elem.bimap(f, f))
        case m: M.Or[T]      => m.copy(elem = m.elem.bimap(f, f))
        case m: M.Many[T]    => m.copy(elem = m.elem.map(f))
        case m: M.Except[T]  => m.copy(elem = f(m.elem))
        case m: M.Build[T]   => m
        case m: M.Err[T]     => m
        case m: M.Tag[T]     => m.copy(elem = f(m.elem))
        case m: M.Cls[T]     => m.copy(elem = f(m.elem))
        case m: M.Tok[T]     => m
        case m: M.Blank[T]   => m
        case m: M.Var[T]     => m
        case m: M.Cons[T]    => m
        case m: M.Opr[T]     => m
        case m: M.Mod[T]     => m
        case m: M.Num[T]     => m
        case m: M.Text[T]    => m
        case m: M.Block[T]   => m
        case m: M.Macro[T]   => m
        case m: M.Invalid[T] => m
      }

    def isValid: Boolean = {
      var out = true
      this.mapStruct {
        case m: M.Err[_] => out = false; m
        case m           => m
      }
      out
    }
  }
  object MatchOf {
    import cats.implicits._

    implicit def reprMatch[T: Repr]: Repr[MatchOf[T]] =
      _.map(Repr(_)).fold
    implicit def ftorMatch: Functor[MatchOf]  = _MatchOf.ftorMatch
    implicit def travMatch: Traverse[MatchOf] = _MatchOf.travMatch
    implicit def foldMatch: Foldable[MatchOf] = _MatchOf.foldMatch

    implicit def offZipMatch[T: Repr]: AST.OffsetZip[MatchOf, T] = t => {
      val s  = t.map(Shifted(0, _))
      val s2 = mapWithOff(s) { case (i, el) => Shifted(i, el.el) }
      val s3 = s2.map(t => (Index(t.off), t.el))
      s3
    }

    val M = Match
    // format: off
    def mapWithOff[T:Repr](self:MatchOf[T])(f: (Int,T) => T): MatchOf[T] =
      mapWithOff_(self)(f,0)._1
    
    def mapWithOff_[T:Repr](self:MatchOf[T])(f: (Int,T) => T, off:Int): (MatchOf[T], Int) = self match {
      case m: M.Build[T]   => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Err[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Tok[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Blank[T]   => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Var[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Cons[T]    => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Opr[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Mod[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Num[T]     => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Text[T]    => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Block[T]   => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Macro[T]   => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: M.Invalid[T] => (m.copy(elem = f(off,m.elem)), off + Repr(m.elem).span)
      case m: Pattern.MatchOf[T] =>
        var loff = off
        val out  = m.mapStructShallow {p =>
          val (nmatch, noff) = mapWithOff_(p)(f, loff)
          loff = noff
          nmatch
        }
        (out, loff)
    }
    // format: on
  }
  object _MatchOf {
    def ftorMatch: Functor[MatchOf]  = semi.functor
    def travMatch: Traverse[MatchOf] = semi.traverse[MatchOf]
    def foldMatch: Foldable[MatchOf] = {
      semi.foldable[MatchOf]
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//// API ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

sealed trait Pattern {
  import Pattern._

  implicit class OptionWhen(v: Option.type) {
    def when[A](cond: Boolean)(a: => A): Option[A] = if (cond) Some(a) else None
  }

  ////////////////////////////
  //// Smart Constructors ////
  ////////////////////////////

  def ::(that: Pattern): Pattern = Seq(that, this)
  def !(that: Pattern):  Pattern = Except(that, this)
  def |(that: Pattern):  Pattern = Or(this, that)
  def |(msg: String):    Pattern = this | Err(msg, Nothing())
  def |?(tag: String):   Pattern = Tag(tag, this)

  def or(that: Pattern):  Pattern = Or(this, that)
  def or(msg: String):    Pattern = this | Err(msg, Nothing())
  def err(msg: String):   Pattern = Err(msg, this)
  def but(pat: Pattern):  Pattern = Except(pat, this)
  def many:               Pattern = Many(this)
  def many1:              Pattern = this :: this.many
  def tag(tag: String):   Pattern = Tag(tag, this)
  def opt:                Pattern = this | Nothing()
  def build:              Pattern = Build(this)
  def till(end: Pattern): Pattern = this.but(end).many
  def tillEnd:            Pattern = this :: End() // fixme: rename
  def fromBegin:          Pattern = Begin() :: this

  def matchRevUnsafe(
    stream: AST.Stream,
    lineBegin: Boolean = false
  ): Match.Result =
    this.matchUnsafe(stream, lineBegin = lineBegin, reversed = true)

  //////////////////////////////////
  //// Pattern Match Resolution ////
  //////////////////////////////////

  /** Unsafe variant of AST Macro tokens pattern matching. If you want to use
    * patterns that could not match all input tokens, use [[matchOpt]] instead.
    */
  def matchUnsafe(
    stream: AST.Stream,
    lineBegin: Boolean = false,
    reversed: Boolean  = false
  ): Match.Result = {
    matchOpt(stream, lineBegin, reversed).getOrElse {
      val msg = "Internal error: template pattern segment was unmatched"
      throw new Error(msg)
    }
  }

  /** This function takes a pattern and applies it to AST input stream. The
    * optional parameter 'reversed' is used for prefix (reverse) matching and is
    * used for prefix macro matching. The function assumes that the pattern does
    * not fail.
    */
  def matchOpt(
    stream0: AST.Stream,
    lineBegin: Boolean,
    reversed: Boolean
  ): Option[Match.Result] = {

    val P = Pattern
    val M = Match

    def matchList(p: Pattern, stream: AST.Stream): (List[Match], AST.Stream) = {
      @tailrec
      def go(
        stream: AST.Stream,
        revOut: List[Match]
      ): (List[Match], AST.Stream) =
        step(p, stream) match {
          case None    => (revOut.reverse, stream)
          case Some(t) => go(t.stream, t.elem :: revOut)
        }
      go(stream, Nil)
    }

    def stepWith(p: Pattern, stream: AST.Stream)(
      f: Match => Match
    ): Option[Match.Result] = step(p, stream).map(_.map(f))

    def step(p: Pattern, stream: AST.Stream): Option[Match.Result] = {

      def out(m: Match, s: AST.Stream)               = Match.Result(m, s)
      def ret(m: Match, s: AST.Stream)               = Some(Match.Result(m, s))
      def ret_(m: Match)                             = Some(Match.Result(m, stream))
      def retIf(b: Boolean)(m: Match, s: AST.Stream) = Option.when(b)(out(m, s))
      def retIf_(b: Boolean)(m: Match)               = retIf(b)(m, stream)

      def matchByCls_[T: AST.UnapplyByType](
        spaced: Pattern.Spaced,
        f: Shifted[T] => Match
      ) = matchByCls[T](spaced)(a => Some(f(a)))

      def matchByCls[T](spaced: Pattern.Spaced)(
        f: Shifted[T] => Option[Match]
      )(implicit pat: AST.UnapplyByType[T]): Option[Match.Result] =
        stream match {
          case Shifted(off, pat(t)) :: ss =>
            val ok = spaced match {
              case None => true
              case Some(s) =>
                val isBlock = t match {
                  case AST.Block.any(_) => true
                  case _                => false
                }
                (s == (off > 0)) && (!isBlock)
            }
            if (ok) f(Shifted(off, t)).map(out(_, ss)) else None
          case _ => None
        }

      p match {

        //// Boundary Matches ////

        case p @ P.Begin() => retIf_(lineBegin)(M.Begin(p))
        case p @ P.End()   => retIf_(stream.isEmpty)(M.End(p))

        //// Structural Matches ////

        case p @ P.Nothing() => ret_(M.Nothing(p))
        case p @ P.Seq(p1, p2) =>
          for {
            r1 <- step(p1, stream)
            r2 <- step(p2, r1.stream)
          } yield out(M.Seq(p, (r1.elem, r2.elem)), r2.stream)

        case p @ P.Or(p1, p2) =>
          val m1 = stepWith(p1, stream)(r => M.Or(p, Left(r)))
          m1.orElse(stepWith(p2, stream)(r => M.Or(p, Right(r))))

        case p @ P.Many(p1) =>
          val (lst, rest) = matchList(p1, stream)
          ret(M.Many(p, lst), rest)

        case p @ P.Except(p1, p2) =>
          step(p1, stream) match {
            case Some(_) => None
            case None    => stepWith(p2, stream)(M.Except(p, _))
          }

        //// Meta Matches ////

        // When performing reverse pattern match, tokens use right-offsets
        // instead of left ones, so we need to push them back before computing
        // AST.
        case p @ P.Build(p1) =>
          stepWith(p1, stream) { patMatch =>
            val stream = patMatch.toStream
            val ast =
              if (!reversed) buildASTFrom(stream).get
              else {
                val (shiftedStream, off) = streamShift(0, stream.reverse)
                val shiftedAst           = buildASTFrom(shiftedStream).get
                shiftedAst.copy(off = off)
              }
            M.Build(p, ast)
          }

        case p @ P.Err(msg, p1) =>
          step(p1, stream).map {
            _.map(
              m => M.Err(p, Shifted(AST.Invalid.Unexpected(msg, m.toStream)))
            )
          }

        case p @ P.Tag(_, p1) => stepWith(p1, stream)(M.Tag(p, _))
        case p @ P.Cls(_, p1) => stepWith(p1, stream)(M.Cls(p, _))

        //// Token Matches ////

        case p @ P.Tok(spaced, tok) =>
          stream match {
            case Shifted(off, t) :: ss =>
              val ok = spaced.forall(_ == (off > 0))
              Option.when(tok == t && ok)(out(M.Tok(p, Shifted(off, t)), ss))
            case _ => None
          }

        case p @ P.Blank(spaced) =>
          matchByCls_[AST.Blank](spaced, M.Blank(p, _))
        case p @ P.Var(spaced)  => matchByCls_[AST.Var](spaced, M.Var(p, _))
        case p @ P.Cons(spaced) => matchByCls_[AST.Cons](spaced, M.Cons(p, _))
        case p @ P.Num(spaced)  => matchByCls_[AST.Number](spaced, M.Num(p, _))
        case p @ P.Text(spaced) => matchByCls_[AST.Text](spaced, M.Text(p, _))
        case p @ P.Block(spaced) =>
          matchByCls_[AST.Block](spaced, M.Block(p, _))
        case p @ P.Opr(spaced, maxPrec) =>
          matchByCls[AST.Opr](spaced) { sast =>
            Option.when(maxPrec.forall(_ >= sast.el.prec))(M.Opr(p, sast))
          }
        case p @ P.Mod(spaced) => matchByCls_[AST.Mod](spaced, M.Mod(p, _))

        case p @ P.Macro(spaced) =>
          matchByCls_[AST.Macro](spaced, M.Macro(p, _))

        case p @ P.Invalid(spaced) =>
          matchByCls_[AST.Invalid](spaced, M.Invalid(p, _))

      }
    }
    step(this, stream0)
  }
}

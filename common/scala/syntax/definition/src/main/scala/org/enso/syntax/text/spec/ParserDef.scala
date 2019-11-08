package org.enso.syntax.text.spec

import org.enso.data.List1
import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.flexer.State
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.syntax.text.AST

import scala.annotation.tailrec

case class ParserDef() extends flexer.Parser[AST.Module] {
  import ParserDef2._

  final def unwrap[T](opt: Option[T]): T = opt match {
    case None    => throw new Error("Internal Error")
    case Some(t) => t
  }

  /////////////
  //// API ////
  /////////////

  override def run(input: Reader): Result[AST.Module] = {
    state.begin(block.MODULE)
    super.run(input)
  }

  ///////////////////////////////////
  //// Basic Char Classification ////
  ///////////////////////////////////

  val lowerLetter: Pattern = range('a', 'z')
  val upperLetter: Pattern = range('A', 'Z')
  val digit: Pattern       = range('0', '9')
  val hex: Pattern         = digit | range('a', 'f') | range('A', 'F')
  val alphaNum: Pattern    = digit | lowerLetter | upperLetter
  val whitespace0: Pattern = ' '.many
  val space: Pattern       = ' '.many1
  val newline: Pattern     = '\n'

  ////////////////
  //// Result ////
  ////////////////

  override def getResult() = result.current.flatMap {
    case AST.Module.any(mod) => Some(mod)
    case _                   => None
  }

  final object result {

    var current: Option[AST]     = None
    var stack: List[Option[AST]] = Nil

    def push(): Unit = logger.trace {
      logger.log(s"Pushed: $current")
      stack +:= current
      current = None
    }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
      logger.log(s"New result: ${current.map(_.show).getOrElse("None")}")
    }

    def app(fn: String => AST): Unit =
      app(fn(currentMatch))

    def app(ast: AST): Unit = logger.trace {
      current = Some(current match {
        case None    => ast
        case Some(r) => AST.App.Prefix(r, off.use(), ast)
      })
    }

    def last(): Option[AST] = {
      @tailrec
      def go(ast: AST): AST = ast match {
        case AST.App.Prefix.any(t) => go(t.arg)
        case t                     => t
      }
      current.map(go)
    }
  }

  ////////////////
  //// Offset ////
  ////////////////

  final object off {
    var current: Int     = 0
    var stack: List[Int] = Nil

    def push(): Unit = logger.trace {
      stack +:= current
      current = 0
    }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
      logger.log(s"New offset: $current")
    }

    def use(): Int = logger.trace {
      val offset = current
      current = 0
      offset
    }

    def on(): Unit = on(0)

    def on(shift: Int): Unit = logger.trace {
      val diff = currentMatch.length + shift
      current += diff
      logger.log(s"lastOffset + $diff = $current")
    }
  }

  ////////////////////
  //// IDENTIFIER ////
  ////////////////////

  final object ident {

    var current: Option[AST.Ident] = None

    def on(cons: String => AST.Ident): Unit = logger.trace_ {
      on(cons(currentMatch))
    }

    def on(ast: AST.Ident): Unit = logger.trace {
      current = Some(ast)
      state.begin(SFX_CHECK)
    }

    def submit(): Unit = logger.trace {
      result.app(unwrap(current))
      current = None
    }

    def onErrSfx(): Unit = logger.trace {
      val ast = AST.Ident.InvalidSuffix(unwrap(current), currentMatch)
      result.app(ast)
      current = None
      state.end()
    }

    def onNoErrSfx(): Unit = logger.trace {
      submit()
      state.end()
    }

    def finalizer(): Unit = logger.trace {
      if (current.isDefined) submit()
    }

    val char: Pattern   = alphaNum | '_'
    val body: Pattern   = char.many >> '\''.many
    val _var: Pattern   = lowerLetter >> body
    val cons: Pattern   = upperLetter >> body
    val breaker: String = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\"
    val errSfx: Pattern = noneOf(breaker).many1

    val SFX_CHECK = state.define("Identifier Suffix Check")
  }

  ROOT            || ident._var   || ident.on(AST.Var(_))
  ROOT            || ident.cons   || ident.on(AST.Cons(_))
  ROOT            || "_"          || ident.on(AST.Blank())
  ident.SFX_CHECK || ident.errSfx || ident.onErrSfx()
  ident.SFX_CHECK || always       || ident.onNoErrSfx()

  //////////////////
  //// Operator ////
  //////////////////

  final object opr {
    def on(cons: String => AST.Ident): Unit = logger.trace {
      on(cons(currentMatch))
    }

    def onGrp(cons: String => AST.Ident): Unit = logger.trace {
      ident.current = Some(cons(currentMatch))
      ident.onNoErrSfx()
    }

    def onNoMod(cons: String => AST.Ident): Unit = logger.trace {
      onNoMod(cons(currentMatch))
    }

    def on(ast: AST.Ident): Unit = logger.trace {
      ident.current = Some(ast)
      state.begin(MOD_CHECK)
    }

    def onNoMod(ast: AST.Ident): Unit = logger.trace {
      ident.current = Some(ast)
      state.begin(SFX_CHECK)
    }

    def onMod(): Unit = logger.trace {
      val opr = AST.Mod(unwrap(ident.current).asInstanceOf[AST.Opr].name)
      ident.current = Some(opr)
    }

    val char: Pattern     = anyOf("!$%&*+-/<>?^~|:\\")
    val errChar: Pattern  = char | "=" | "," | "."
    val errSfx: Pattern   = errChar.many1
    val body: Pattern     = char.many1
    val opsEq: Pattern    = "=" | "==" | ">=" | "<=" | "/=" | "#="
    val opsDot: Pattern   = "." | ".." | "..." | ","
    val opsGrp: Pattern   = anyOf("()[]{}")
    val opsCmm: Pattern   = "#" | "##"
    val opsNoMod: Pattern = opsEq | opsDot | opsCmm

    val SFX_CHECK = state.define("Operator Suffix Check")
    val MOD_CHECK = state.define("Operator Modifier Check")
    MOD_CHECK.parent = SFX_CHECK
  }

  ROOT          || opr.body     || opr.on(AST.Opr(_))
  ROOT          || opr.opsNoMod || opr.onNoMod(AST.Opr(_))
  ROOT          || opr.opsGrp   || opr.onGrp(AST.Opr(_))
  opr.MOD_CHECK || "="          || opr.onMod()
  opr.SFX_CHECK || opr.errSfx   || ident.onErrSfx()
  opr.SFX_CHECK || always       || ident.onNoErrSfx()

  ////////////////
  //// NUMBER ////
  ////////////////

  final object num {

    var part1: String = ""
    var part2: String = ""

    def reset(): Unit = logger.trace {
      part1 = ""
      part2 = ""
    }

    def submit(): Unit = logger.trace {
      val base = if (part1 == "") None else Some(part1)
      result.app(AST.Number(base, part2))
      reset()
    }

    def onDanglingBase(): Unit = logger.trace {
      state.end()
      result.app(AST.Number.DanglingBase(part2))
      reset()
    }

    def onDecimal(): Unit = logger.trace {
      part2 = currentMatch
      state.begin(PHASE2)
    }

    def onExplicitBase(): Unit = logger.trace {
      state.end()
      part1 = part2
      part2 = currentMatch.substring(1)
      submit()
    }

    def onNoExplicitBase(): Unit = logger.trace {
      state.end()
      submit()
    }

    val decimal: Pattern = digit.many1

    val PHASE2: State = state.define("Number Phase 2")
  }

  ROOT       || num.decimal           || num.onDecimal()
  num.PHASE2 || "_" >> alphaNum.many1 || num.onExplicitBase()
  num.PHASE2 || "_"                   || num.onDanglingBase()
  num.PHASE2 || always                || num.onNoExplicitBase()

  //////////////
  //// Text ////
  //////////////

  import AST.Text.Quote

  class TextState(
    var lines: List[AST.Text.LineOf[AST.Text.Segment._Fmt[AST]]],
    var lineBuilder: List[AST.Text.Segment.Fmt],
    val quote: Quote
  )

  final object text {

    val Segment = AST.Text.Segment

    var stack: List[TextState] = Nil
    var current                = new TextState(Nil, Nil, Quote.Single)

    def push(): Unit = logger.trace {
      stack +:= current
    }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
    }

    def submitEmpty(groupIx: State, quoteNum: Quote): Unit = logger.trace {
      if (groupIx == RAW)
        result.app(AST.Text.Raw(AST.Text.Body(quoteNum)))
      else
        result.app(AST.Text.Fmt(AST.Text.Body(quoteNum)))
    }

    def finishCurrent(): AST.Text = logger.trace {
      onSubmitLine()
      val t     = current
      val body  = AST.Text.BodyOf(t.quote, List1(t.lines.reverse).get)
      val isRaw = state.current == RAW
      pop()
      off.pop()
      state.end()
      if (isRaw)
        AST.Text.Raw(body.asInstanceOf[AST.Text.BodyOf[Segment._Raw[AST]]])
      else
        AST.Text.Fmt(body)
    }

    def submit(): Unit = logger.trace {
      result.app(finishCurrent())
    }

    def submit(segment: Segment.Fmt): Unit = logger.trace {
      current.lineBuilder +:= segment
    }

    def submitUnclosed(): Unit = logger.trace {
      result.app(AST.Text.UnclosedOf(finishCurrent()))
    }

    def onBegin(grp: State, quoteSize: Quote): Unit = logger.trace {
      push()
      off.push()
      off.push()
      current = new TextState(Nil, Nil, quoteSize)
      state.begin(grp)
    }

    def submitPlainSegment(): Unit = logger.trace {
      current.lineBuilder = current.lineBuilder match {
        case Segment._Plain(t) :: _ =>
          Segment.Plain(t + currentMatch) :: current.lineBuilder.tail
        case _ => Segment.Plain(currentMatch) :: current.lineBuilder
      }
    }

    def onQuote(quoteSize: Quote): Unit = logger.trace {
      if (current.quote == Quote.Triple
          && quoteSize == Quote.Single)
        submitPlainSegment()
      else if (current.quote == Quote.Single
               && quoteSize == Quote.Triple) {
        val groupIx = state.current
        submit()
        submitEmpty(groupIx, Quote.Single)
      } else
        submit()
    }

    def onEscape(code: Segment.Escape): Unit = logger.trace {
      submit(Segment._Escape(code))
    }

    def onEscapeU16(): Unit = logger.trace {
      val code = currentMatch.drop(2)
      onEscape(Segment.Escape.Unicode.U16(code))
    }

    def onEscapeU32(): Unit = logger.trace {
      val code = currentMatch.drop(2)
      onEscape(Segment.Escape.Unicode.U32(code))
    }

    def onEscapeInt(): Unit = logger.trace {
      val int = currentMatch.drop(1).toInt
      onEscape(Segment.Escape.Number(int))
    }

    def onInvalidEscape(): Unit = logger.trace {
      val str = currentMatch.drop(1)
      onEscape(Segment.Escape.Invalid(str))
    }

    def onEscapeSlash(): Unit = logger.trace {
      onEscape(Segment.Escape.Slash)
    }

    def onEscapeQuote(): Unit = logger.trace {
      onEscape(Segment.Escape.Quote)
    }

    def onEscapeRawQuote(): Unit = logger.trace {
      onEscape(Segment.Escape.RawQuote)
    }

    def onInterpolateBegin(): Unit = logger.trace {
      result.push()
      off.push()
      state.begin(INTERPOLATE)
    }

    def onInterpolateEnd(): Unit = logger.trace {
      if (state.isInside(INTERPOLATE)) {
        state.endTill(INTERPOLATE)
        submit(Segment.Expr(result.current))
        result.pop()
        off.pop()
        state.end()
      } else {
        onUnrecognized()
      }
    }

    def onEOF(): Unit = logger.trace {
      submitUnclosed()
      rewind()
    }

    def onSubmitLine(): Unit = logger.trace {
      off.pop()
      current.lines +:= AST.Text.LineOf(off.use(), current.lineBuilder.reverse)
      current.lineBuilder = Nil
    }

    def onNewLine(): Unit = logger.trace {
      state.end()
      onSubmitLine()
      off.on()
      off.push()
    }

    val fmtChar    = noneOf("'`\\\n")
    val escape_int = "\\" >> num.decimal
    val escape_u16 = "\\u" >> repeat(fmtChar, 0, 4)
    val escape_u32 = "\\U" >> repeat(fmtChar, 0, 8)
    val fmtSeg     = fmtChar.many1
    val rawSeg     = noneOf("\"\n").many1

    val FMT: State         = state.define("Formatted Text")
    val RAW: State         = state.define("Raw Text")
    val NEWLINE: State     = state.define("Text Newline")
    val INTERPOLATE: State = state.define("Interpolate")
    INTERPOLATE.parent = ROOT
  }

  ROOT     || '`'         || text.onInterpolateEnd()
  text.FMT || '`'         || text.onInterpolateBegin()
  ROOT     || "'"         || text.onBegin(text.FMT, Quote.Single)
  ROOT     || "'''"       || text.onBegin(text.FMT, Quote.Triple)
  text.FMT || "'"         || text.onQuote(Quote.Single)
  text.FMT || "'''"       || text.onQuote(Quote.Triple)
  text.FMT || text.fmtSeg || text.submitPlainSegment()
  text.FMT || eof         || text.onEOF()
  text.FMT || '\n'        || state.begin(text.NEWLINE)

  ROOT     || "\""        || text.onBegin(text.RAW, Quote.Single)
  ROOT     || "\"\"\""    || text.onBegin(text.RAW, Quote.Triple)
  text.RAW || "\""        || text.onQuote(Quote.Single)
  text.RAW || "$$$$$"     || {}
  text.RAW || "\"\"\""    || text.onQuote(Quote.Triple)
  text.RAW || text.rawSeg || text.submitPlainSegment()
  text.RAW || eof         || text.onEOF()
  text.RAW || '\n'        || state.begin(text.NEWLINE)

  text.NEWLINE || space.opt || text.onNewLine()

  AST.Text.Segment.Escape.Character.codes.foreach { code =>
    val char = s"text.Segment.Escape.Character.$code"
    text.FMT || s"\\$code" run s"text.onEscape($char)"
  }

  AST.Text.Segment.Escape.Control.codes.foreach { code =>
    val ctrl = s"text.Segment.Escape.Control.$code"
    text.FMT || s"\\$code" run s"text.onEscape($ctrl)"
  }

  text.FMT || text.escape_u16        || text.onEscapeU16()
  text.FMT || text.escape_u32        || text.onEscapeU32()
  text.FMT || text.escape_int        || text.onEscapeInt()
  text.FMT || "\\\\"                 || text.onEscapeSlash()
  text.FMT || "\\'"                  || text.onEscapeQuote()
  text.FMT || "\\\""                 || text.onEscapeRawQuote()
  text.FMT || ("\\" >> text.fmtChar) || text.onInvalidEscape()
  text.FMT || "\\"                   || text.submitPlainSegment()

  //////////////
  /// Blocks ///
  //////////////

  // because of bug in macroContext.eval it cannot be part of object block
  class BlockState(
    val isOrphan: Boolean,
    var isValid: Boolean,
    var indent: Int,
    var emptyLines: List[Int],
    var firstLine: Option[AST.Block.Line.NonEmpty],
    var lines: List[AST.Block.OptLine]
  )

  final object block {

    var stack: List[BlockState] = Nil
    var emptyLines: List[Int]   = Nil
    var current: BlockState     = new BlockState(false, true, 0, Nil, None, Nil)

    def push(newIndent: Int, orphan: Boolean): Unit =
      logger.trace {
        stack +:= current
        current =
          new BlockState(orphan, true, newIndent, emptyLines.reverse, None, Nil)
        emptyLines = Nil
      }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
    }

    def build(): AST.Block = logger.trace {
      submitLine()
      AST.Block(
        current.isOrphan,
        AST.Block.Continuous,
        current.indent,
        current.emptyLines,
        unwrap(current.firstLine),
        current.lines.reverse
      )
    }

    def submit(): Unit = logger.trace {
      val block = build()
      result.pop()
      off.pop()
      pop()
      val block2 = result.last() match {
        case None => block
        case Some(ast) =>
          ast match {
            case AST.Opr.any(_) =>
              block.replaceType(AST.Block.Discontinuous): AST.Block
            case _ => block

          }
      }
      result.app(block2)
      off.push()
      logger.endGroup()
    }

    def submitModule(): Unit = logger.trace {
      val body = current.firstLine match {
        case None       => current.lines.reverse
        case Some(line) => line.toOptional +: current.lines.reverse
      }
      val line :: lines = (current.emptyLines
          .map(AST.Block.Line(None, _)): List[AST.Block.OptLine]) ++ body
      val module = AST.Module(line, lines)
      result.current = Some(module)
      logger.endGroup()
    }

    def submitLine(): Unit = logger.trace {
      result.current match {
        case None =>
        case Some(r) =>
          off.pop()
          current.firstLine match {
            case None =>
              current.firstLine = Some(AST.Block.Line.Required(r, off.use()))
            case Some(_) =>
              current.lines +:= AST.Block.Line(result.current, off.use())
          }
      }
      emptyLines.reverse.foreach(current.lines +:= AST.Block.OptLine(_))
      emptyLines     = Nil
      result.current = None
    }

    def onEmptyLine(): Unit = logger.trace {
      off.on(-1)
      emptyLines +:= off.use()
    }

    def onModuleBegin(): Unit = logger.trace {
      current.emptyLines = emptyLines.reverse
      emptyLines         = Nil
      rewind()
      off.push()
      state.end()
      state.begin(NEWLINE)
    }

    def onBegin(newIndent: Int): Unit = logger.trace {
      val isOrphan = result.current.isEmpty
      result.push()
      push(newIndent, isOrphan)
      logger.beginGroup()
    }

    def onEOFLine(): Unit = logger.trace {
      state.end()
      submitLine()
      off.on()
      current.lines +:= AST.Block.OptLine(off.use())
      off.pop()
      onEOF()
    }

    def onEndLine(): Unit = logger.trace {
      off.push()
      state.begin(NEWLINE)
    }

    def onNewLine(): Unit = logger.trace {
      state.end()
      off.on()
      if (off.current == current.indent)
        submitLine()
      else if (off.current > current.indent)
        onBegin(off.use())
      else
        onEnd(off.use())
      state.begin(FIRSTCHAR)
    }

    def onEnd(newIndent: Int): Unit = logger.trace {
      while (newIndent < current.indent) submit()
      if (newIndent > current.indent) {
        logger.log("Block with invalid indentation")
        onBegin(newIndent)
        current.isValid = false
      } else {
        off.push()
        submitLine()
      }
    }

    val MODULE    = state.define("Module")
    val NEWLINE   = state.define("Newline")
    val FIRSTCHAR = state.define("First Char")
  }

  ROOT            || newline              || block.onEndLine()
  block.NEWLINE   || space.opt >> newline || block.onEmptyLine()
  block.NEWLINE   || space.opt >> eof     || block.onEOFLine()
  block.NEWLINE   || space.opt            || block.onNewLine()
  block.MODULE    || space.opt >> newline || block.onEmptyLine()
  block.MODULE    || space.opt            || block.onModuleBegin()
  block.FIRSTCHAR || always               || state.end()

  ////////////////
  /// Defaults ///
  ////////////////

  final def onUnrecognized(): Unit = logger.trace {
    result.app(AST.Invalid.Unrecognized(_))
  }

  final def onEOF(): Unit = logger.trace {
    ident.finalizer()
    off.push()
    block.submitLine()
    block.onEnd(0)
    block.submitModule()
  }

  ROOT || space || off.on()
  ROOT || eof   || onEOF()
  ROOT || any   || onUnrecognized()
}

object ParserDef2 {
  type Result[T] = flexer.Parser.Result[T]
}

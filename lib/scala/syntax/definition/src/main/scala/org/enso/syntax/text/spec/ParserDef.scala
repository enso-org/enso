package org.enso.syntax.text.spec

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.flexer.State
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.syntax.text.{AST, Shape}

import scala.annotation.tailrec

case class ParserDef() extends flexer.Parser[AST.Module] {
  import ParserDef2._

  final def unwrap[T](opt: Option[T]): T =
    opt match {
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
  val alphaNum: Pattern    = digit | lowerLetter | upperLetter
  val space: Pattern       = ' '.many1
  val newline: Pattern     = '\n'
  val emptyLine: Pattern   = ' '.many >> newline

  ////////////////
  //// Result ////
  ////////////////

  override def getResult() =
    result.current.flatMap {
      case AST.Module.any(mod) => Some(mod)
      case _                   => None
    }

  final object result {

    var current: Option[AST]     = None
    var stack: List[Option[AST]] = Nil

    def push(): Unit =
      logger.trace {
        logger.log(s"Pushed: $current")
        stack +:= current
        current = None
      }

    def pop(): Unit =
      logger.trace {
        current = stack.head
        stack   = stack.tail
        logger.log(s"New result: ${current.map(_.show()).getOrElse("None")}")
      }

    def app(fn: String => AST): Unit =
      app(fn(currentMatch))

    def app(ast: AST): Unit =
      logger.trace {
        current = Some(current match {
          case None    => ast
          case Some(r) => AST.App.Prefix(r, off.use(), ast)
        })
      }

    def last(): Option[AST] = {
      @tailrec
      def go(ast: AST): AST =
        ast match {
          case AST.App.Prefix.any(t) => go(t.arg)
          case t                     => t
        }
      current.map(go)
    }

    def takeLast(): Option[AST] = {
      current.map(c => {
        val app = c.asInstanceOf[AST.App.Prefix]
        current = Some(app.func)
        app.arg
      })
    }
  }

  ////////////////
  //// Offset ////
  ////////////////

  final object off {
    var current: Int     = 0
    var stack: List[Int] = Nil

    def push(): Unit =
      logger.trace {
        stack +:= current
        current = 0
      }

    def pop(): Unit =
      logger.trace {
        current = stack.head
        stack   = stack.tail
        logger.log(s"New offset: $current")
      }

    def use(): Int =
      logger.trace {
        val offset = current
        current = 0
        offset
      }

    def on(): Unit = on(0)

    def on(shift: Int): Unit =
      logger.trace {
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

    def on(cons: String => AST.Ident): Unit =
      logger.trace_ {
        on(cons(currentMatch))
      }

    def on(ast: AST.Ident): Unit =
      logger.trace {
        current = Some(ast)
        state.begin(SFX_CHECK)
      }

    def submit(): Unit =
      logger.trace {
        result.app(unwrap(current))
        current = None
      }

    def onErrSfx(): Unit =
      logger.trace {
        val ast = AST.Ident.InvalidSuffix(unwrap(current), currentMatch)
        result.app(ast)
        current = None
        state.end()
      }

    def onNoErrSfx(): Unit =
      logger.trace {
        submit()
        state.end()
      }

    def finalizer(): Unit =
      logger.trace {
        if (current.isDefined) submit()
      }

    val char: Pattern       = alphaNum | '_'
    val body: Pattern       = char.many >> '\''.many
    val _var: Pattern       = lowerLetter >> body
    val cons: Pattern       = upperLetter >> body
    val annotation: Pattern = "@" >> cons
    val breaker: String     = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\"
    val errSfx: Pattern     = noneOf(breaker).many1

    val SFX_CHECK = state.define("Identifier Suffix Check")
  }

  ROOT            || ident._var       || ident.on(AST.Var(_))
  ROOT            || ident.cons       || ident.on(AST.Cons(_))
  ROOT            || "_"              || ident.on(AST.Blank())
  ROOT            || ident.annotation || ident.on(AST.Annotation(_))
  ident.SFX_CHECK || ident.errSfx     || ident.onErrSfx()
  ident.SFX_CHECK || always           || ident.onNoErrSfx()

  //////////////////
  //// Operator ////
  //////////////////

  final object opr {
    def on(cons: String => AST.Ident): Unit =
      logger.trace {
        on(cons(currentMatch))
      }

    def onGrp(cons: String => AST.Ident): Unit =
      logger.trace {
        ident.current = Some(cons(currentMatch))
        ident.onNoErrSfx()
      }

    def onNoMod(cons: String => AST.Ident): Unit =
      logger.trace {
        onNoMod(cons(currentMatch))
      }

    def on(ast: AST.Ident): Unit =
      logger.trace {
        ident.current = Some(ast)
        state.begin(MOD_CHECK)
      }

    def onNoMod(ast: AST.Ident): Unit =
      logger.trace {
        ident.current = Some(ast)
        state.begin(SFX_CHECK)
      }

    def onMod(): Unit =
      logger.trace {
        val opr = AST.Mod(unwrap(ident.current).asInstanceOf[AST.Opr].name)
        ident.current = Some(opr)
      }

    def onDottedOpr(): Unit = {
      logger.trace {
        ident.current = Some(AST.Ident.Opr("."))
        ident.submit()
        ident.current = Some(AST.Ident.Var(currentMatch.substring(1)))
        ident.submit()
      }
    }

    val char: Pattern      = anyOf(";!$%&*+-/<>?^~|:\\")
    val errChar: Pattern   = char | "=" | "," | "."
    val errSfx: Pattern    = errChar.many1
    val body: Pattern      = char.many1
    val opsEq: Pattern     = "=" | "==" | ">=" | "<=" | "/=" | "#=" | "!="
    val dot: Pattern       = "."
    val dottedOps: Pattern = dot >> (body | opsEq)
    val opsDot: Pattern    = dot | ".." | "..." | ","
    val opsGrp: Pattern    = anyOf("()[]{}")
    val opsCmm: Pattern    = "#" | "##"
    val opsNoMod: Pattern  = opsEq | opsDot | opsCmm

    val SFX_CHECK = state.define("Operator Suffix Check")
    val MOD_CHECK = state.define("Operator Modifier Check")
    MOD_CHECK.parent = SFX_CHECK
  }

  ROOT          || opr.body      || opr.on(AST.Opr(_))
  ROOT          || opr.dottedOps || opr.onDottedOpr()
  ROOT          || opr.opsNoMod  || opr.onNoMod(AST.Opr(_))
  ROOT          || opr.opsGrp    || opr.onGrp(AST.Opr(_))
  opr.MOD_CHECK || "="           || opr.onMod()
  opr.SFX_CHECK || opr.errSfx    || ident.onErrSfx()
  opr.SFX_CHECK || always        || ident.onNoErrSfx()

  ////////////////
  //// NUMBER ////
  ////////////////

  final object num {

    var part1: String = ""
    var part2: String = ""

    def reset(): Unit =
      logger.trace {
        part1 = ""
        part2 = ""
      }

    def submit(): Unit =
      logger.trace {
        val base = if (part1 == "") None else Some(part1)
        result.app(AST.Number(base, part2))
        reset()
      }

    def onDanglingBase(): Unit =
      logger.trace {
        state.end()
        result.app(AST.Number.DanglingBase(part2))
        reset()
      }

    def onDecimal(): Unit =
      logger.trace {
        part2 = currentMatch
        state.begin(PHASE2)
      }

    def onExplicitBase(): Unit =
      logger.trace {
        state.end()
        part1 = part2
        part2 = currentMatch.substring(1)
        submit()
      }

    def onNoExplicitBase(): Unit =
      logger.trace {
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

  class TextState(
    var offset: Int,
    var spaces: Int,
    var lines: List[AST.Text.Block.Line[AST.Text.Segment.Fmt]],
    var emptyLines: List[Int],
    var lineBuilder: List[AST.Text.Segment.Fmt]
  )

  final object text {

    import AST.Text.Block.Line

    val Segment = AST.Text.Segment

    var stack: List[TextState] = Nil
    var text: TextState        = _

    def push(): Unit =
      logger.trace {
        stack +:= text
      }

    def pop(): Unit =
      logger.trace {
        text  = stack.head
        stack = stack.tail
      }

    def onInvalidQuote(): Unit =
      logger.trace {
        result.app(AST.Text.InvalidQuote(currentMatch))
      }

    def onInlineBlock(): Unit =
      logger.trace {
        result.app(AST.Text.InlineBlock(currentMatch))
      }

    def finish(
      raw: List[Line[Segment.Raw]] => AST,
      fmt: List[Line[Segment.Fmt]] => AST
    ): Unit =
      logger.trace {
        submitLine()
        val isFMT = state.current.parent.contains(FMT)
        val body  = text.lines.reverse
        val t =
          if (isFMT) fmt(body)
          else raw(body.asInstanceOf[List[Line[Segment.Raw]]])
        pop()
        state.end()
        result.app(t)
      }

    def submit(segment: Segment.Fmt): Unit =
      logger.trace {
        text.lineBuilder +:= segment
      }

    def submit(): Unit =
      logger.trace {
        finish(
          t => AST.Text.Raw(t.head.text: _*),
          t => AST.Text(t.head.text: _*)
        )
      }

    def submitMissingQuote(): Unit =
      logger.trace {
        rewind()
        submitUnclosed()
      }

    def submitInvalidQuote(): Unit =
      logger.trace {
        submitUnclosed()
        onInvalidQuote()
      }

    def submitUnclosed(): Unit =
      logger.trace {
        val Text = AST.Text.Unclosed
        finish(t => Text.Raw(t.head.text: _*), t => Text(t.head.text: _*))
      }

    def onEndOfBlock(): Unit =
      logger.trace {
        if (text.lineBuilder.isEmpty)
          block.emptyLines = text.emptyLines ++ block.emptyLines
        val (s, o) = (text.spaces, text.offset)
        finish(t => AST.Text.Raw(s, o, t: _*), t => AST.Text(s, o, t: _*))
        off.push()
        rewind()
      }

    def onBegin(grp: State): Unit =
      logger.trace {
        push()
        state.begin(grp)
        text = new TextState(0, 0, Nil, Nil, Nil)
      }

    def onBeginBlock(grp: State): Unit =
      logger.trace {
        val offset = if (state.current == block.FIRSTCHAR) {
          state.end()
          block.current.offset
        } else
          OFFSET_OF_FIRST_LINE_FOUND
        if (currentMatch.last == '\n') {
          onBegin(grp)
          text.offset = offset
          text.spaces = currentMatch.length - BLOCK_QUOTE_SIZE - 1
          state.begin(NEWLINE)
        } else {
          val spaces = currentMatch.length - BLOCK_QUOTE_SIZE
          result.app(
            if (grp == FMT_BLCK) AST.Text(spaces = spaces, offset)
            else AST.Text.Raw(spaces             = spaces, offset)
          )
          onEOF()
        }
      }

    def submitPlainSegment(): Unit =
      logger.trace {
        text.lineBuilder = text.lineBuilder match {
          case Shape.SegmentPlain(t) :: _ =>
            Segment.Plain(t + currentMatch) :: text.lineBuilder.tail
          case _ => Segment.Plain(currentMatch) :: text.lineBuilder
        }
      }

    def onEscape(code: Segment.Escape): Unit =
      logger.trace {
        submit(Shape.SegmentEscape(code))
      }

    def onEscape(code: Segment.RawEscape): Unit =
      logger.trace {
        submit(Shape.SegmentRawEscape(code))
      }

    def onEscapeU21(): Unit =
      logger.trace {
        val code = currentMatch.drop(3).dropRight(1)
        onEscape(Segment.Escape.Unicode.U21(code))
      }

    def onEscapeU16(): Unit =
      logger.trace {
        val code = currentMatch.drop(2)
        onEscape(Segment.Escape.Unicode.U16(code))
      }

    def onEscapeU32(): Unit =
      logger.trace {
        val code = currentMatch.drop(2)
        onEscape(Segment.Escape.Unicode.U32(code))
      }

    def onEscapeInt(): Unit =
      logger.trace {
        val int = currentMatch.drop(1).toInt
        onEscape(Segment.Escape.Number(int))
      }

    def onEscapeInvalid(): Unit =
      logger.trace {
        val chr = currentMatch.charAt(1)
        onEscape(Segment.Escape.Invalid(chr))
      }

    def onEscapeUnfinished(): Unit =
      logger.trace {
        onEscape(Segment.Escape.Unfinished)
      }

    def onEscapeSlash(): Unit =
      logger.trace {
        onEscape(Segment.Escape.Slash)
      }

    def onEscapeQuote(): Unit =
      logger.trace {
        onEscape(Segment.Escape.Quote)
      }

    def onEscapeRawQuote(): Unit =
      logger.trace {
        onEscape(Segment.Escape.RawQuote)
      }

    def onInterpolateBegin(): Unit =
      logger.trace {
        result.push()
        off.push()
        state.begin(INTERPOLATE)
      }

    def onInterpolateEnd(): Unit =
      logger.trace {
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

    def submitLine(): Unit =
      logger.trace {
        if (
          state.current == FMT_LINE || state.current == RAW_LINE || text.lineBuilder.nonEmpty
        ) {
          val Line = Shape.TextBlockLine
          text.lines +:= Line(text.emptyLines.reverse, text.lineBuilder.reverse)
          text.lineBuilder = Nil
          text.emptyLines  = Nil
        }
      }

    def onEndOfLine(): Unit =
      logger.trace {
        state.begin(NEWLINE)
        submitLine()
      }

    def onNewLine(): Unit =
      logger.trace {
        state.end()
        if (text.offset == OFFSET_OF_FIRST_LINE_FOUND)
          text.offset = currentMatch.length
        val leadingSpaces = currentMatch.length - text.offset
        if (leadingSpaces < 0) {
          onEndOfBlock()
          state.begin(block.NEWLINE)
        } else if (leadingSpaces != 0)
          text.lineBuilder +:= Segment.Plain(" " * leadingSpaces)
      }

    def onEmptyLine(): Unit =
      logger.trace {
        text.emptyLines :+= currentMatch.length - 1
      }

    def onEOFNewLine(): Unit =
      logger.trace {
        state.end()
        onEndOfBlock()
        state.begin(block.NEWLINE)
      }

    val BLOCK_QUOTE_SIZE           = 3
    val OFFSET_OF_FIRST_LINE_FOUND = -1

    val fmtBlock   = "'''" >> space.opt >> (eof | newline)
    val rawBlock   = "\"\"\"" >> space.opt >> (eof | newline)
    val fmtChar    = noneOf("'`\\\n")
    val escapeChar = noneOf("'\\\"`\\\\\\n\\r{}")
    val escape_int = "\\" >> num.decimal
    val escape_u21 = "\\u{" >> repeat(escapeChar, 0, 8) >> "}"
    val escape_u16 = "\\u" >> repeat(escapeChar, 0, 4)
    val escape_u32 = "\\U" >> repeat(escapeChar, 0, 8)
    val fmtSeg     = fmtChar.many1
    val rawSeg     = noneOf("\"\n\\").many1
    val fmtBSeg    = noneOf("\n\\`").many1
    val rawBSeg    = noneOf("\n").many1

    val FMT: State         = state.define("Formatted Text")
    val FMT_LINE: State    = state.define("Formatted Line Of Text")
    val RAW_LINE: State    = state.define("Raw Line Of Text")
    val FMT_BLCK: State    = state.define("Formatted Block Of Text")
    val RAW_BLCK: State    = state.define("Raw Block Of Text")
    val NEWLINE: State     = state.define("Text Newline")
    val INTERPOLATE: State = state.define("Interpolate")
    INTERPOLATE.parent = ROOT
    FMT_LINE.parent    = FMT
    FMT_BLCK.parent    = FMT
  }

  ROOT     || '`' || text.onInterpolateEnd()
  text.FMT || '`' || text.onInterpolateBegin()

  ROOT || "'''" >> "'".many1     || text.onInvalidQuote()
  ROOT || "\"\"\"" >> "\"".many1 || text.onInvalidQuote()

  ROOT            || "'"           || text.onBegin(text.FMT_LINE)
  text.FMT_LINE   || "'"           || text.submit()
  text.FMT_LINE   || "'".many1     || text.submitInvalidQuote()
  text.FMT_LINE   || text.fmtSeg   || text.submitPlainSegment()
  text.FMT_LINE   || eof           || text.submitMissingQuote()
  text.FMT_LINE   || newline       || text.submitMissingQuote()
  block.FIRSTCHAR || text.fmtBlock || text.onBeginBlock(text.FMT_BLCK)
  ROOT            || text.fmtBlock || text.onBeginBlock(text.FMT_BLCK)
  ROOT            || "'''"         || text.onInlineBlock()
  text.FMT_BLCK   || text.fmtBSeg  || text.submitPlainSegment()
  text.FMT_BLCK   || eof           || text.onEndOfBlock()
  text.FMT_BLCK   || newline       || text.onEndOfLine()

  ROOT            || '"'           || text.onBegin(text.RAW_LINE)
  text.RAW_LINE   || '"'           || text.submit()
  text.RAW_LINE   || '"'.many1     || text.submitInvalidQuote()
  text.RAW_LINE   || text.rawSeg   || text.submitPlainSegment()
  text.RAW_LINE   || eof           || text.submitMissingQuote()
  text.RAW_LINE   || newline       || text.submitMissingQuote()
  block.FIRSTCHAR || text.rawBlock || text.onBeginBlock(text.RAW_BLCK)
  ROOT            || text.rawBlock || text.onBeginBlock(text.RAW_BLCK)
  ROOT            || "\"\"\""      || text.onInlineBlock()
  text.RAW_BLCK   || text.rawBSeg  || text.submitPlainSegment()
  text.RAW_BLCK   || eof           || text.onEndOfBlock()
  text.RAW_BLCK   || newline       || text.onEndOfLine()

  text.NEWLINE || space.opt            || text.onNewLine()
  text.NEWLINE || space.opt >> newline || text.onEmptyLine()
  text.NEWLINE || space.opt >> eof     || text.onEOFNewLine()

  AST.Text.Segment.Escape.Character.codes.foreach { code =>
    val char = s"text.Segment.Escape.Character.$code"
    text.FMT || s"\\$code" run s"text.onEscape($char)"
  }

  AST.Text.Segment.Escape.Control.codes.foreach { code =>
    val ctrl = s"text.Segment.Escape.Control.$code"
    text.FMT || s"\\$code" run s"text.onEscape($ctrl)"
  }

  text.FMT || text.escape_u21 || text.onEscapeU21()
  text.FMT || text.escape_u16 || text.onEscapeU16()
  text.FMT || text.escape_u32 || text.onEscapeU32()
  text.FMT || text.escape_int || text.onEscapeInt()
  text.FMT || "\\\\"          || text.onEscapeSlash()
  text.FMT || "\\'"           || text.onEscapeQuote()
  text.FMT || "\\" >> any     || text.onEscapeInvalid()
  text.FMT || "\\"            || text.onEscapeUnfinished()

  text.RAW_LINE || "\\\""      || text.onEscapeRawQuote()
  text.RAW_LINE || "\\\\"      || text.onEscapeSlash()
  text.RAW_LINE || "\\" >> any || text.onEscapeInvalid()
  text.RAW_LINE || "\\"        || text.onEscapeUnfinished()

  //////////////
  /// Blocks ///
  //////////////

  // because of bug in macroContext.eval it cannot be part of object block
  class BlockState(
    val isOrphan: Boolean,
    var isValid: Boolean,
    var offset: Int,
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

    def pop(): Unit =
      logger.trace {
        current = stack.head
        stack   = stack.tail
      }

    def build(): AST.Block =
      logger.trace {
        submitLine()
        AST.Block(
          current.isOrphan,
          AST.Block.Continuous,
          current.offset,
          current.emptyLines,
          unwrap(current.firstLine),
          current.lines.reverse
        )
      }

    def submit(): Unit =
      logger.trace {
        val block = build()
        result.pop()
        off.pop()
        pop()
        val block2: AST.Block = result.last() match {
          case Some(AST.Opr.any(_)) =>
            block.replaceType(AST.Block.Discontinuous)
          case _ => block
        }
        result.app(block2)
        off.push()
        logger.endGroup()
      }

    def submitModule(): Unit =
      logger.trace {
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

    def submitLine(): Unit =
      logger.trace {
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

    def onEmptyLine(): Unit =
      logger.trace {
        off.on(-1)
        emptyLines +:= off.use()
      }

    def onModuleBegin(): Unit =
      logger.trace {
        current.emptyLines = emptyLines.reverse
        emptyLines         = Nil
        rewind()
        off.push()
        state.end()
        state.begin(NEWLINE)
      }

    def onBegin(newIndent: Int): Unit =
      logger.trace {
        val isOrphan = result.current.isEmpty
        result.push()
        push(newIndent, isOrphan)
        logger.beginGroup()
      }

    def onEOFLine(): Unit =
      logger.trace {
        state.end()
        submitLine()
        off.on()
        current.lines +:= AST.Block.OptLine(off.use())
        off.pop()
        onEOF()
      }

    def onEndLine(): Unit =
      logger.trace {
        off.push()
        state.begin(NEWLINE)
      }

    def onNewLine(): Unit =
      logger.trace {
        state.end()
        off.on()
        if (off.current == current.offset)
          submitLine()
        else if (off.current > current.offset)
          onBegin(off.use())
        else
          onEnd(off.use())
        state.begin(FIRSTCHAR)
      }

    def onEnd(newIndent: Int): Unit =
      logger.trace {
        while (newIndent < current.offset) submit()
        if (newIndent > current.offset) {
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

  ROOT            || newline          || block.onEndLine()
  block.NEWLINE   || emptyLine        || block.onEmptyLine()
  block.NEWLINE   || space.opt >> eof || block.onEOFLine()
  block.NEWLINE   || space.opt        || block.onNewLine()
  block.MODULE    || emptyLine        || block.onEmptyLine()
  block.MODULE    || space.opt        || block.onModuleBegin()
  block.FIRSTCHAR || always           || state.end()

  ////////////////
  /// Defaults ///
  ////////////////

  final def onUnrecognized(): Unit =
    logger.trace {
      result.app(AST.Invalid.Unrecognized(_))
    }

  final def onEOF(): Unit =
    logger.trace {
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

package org.enso.syntax.text.spec

import org.enso.flexer
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.flexer.{Reader, State}
import org.enso.syntax.text.AST

case class ParserDefSmall() extends flexer.Parser[AST.Module] {
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
    state.begin(ROOT)
    super.run(input)
  }

  // === Debug Lexer Definition ===

  /* a-word   = 'a'+;
   * b-word   = 'b'+;
   * word     = a-word | b-word;
   * space    = ' ';
   * language = word, (space, word)*;
   */

  val aWord: Pattern       = 'a'.many1
  val bWord: Pattern       = 'b'.many1
  val space: Pattern       = ' '
  val spacedAWord: Pattern = space >> aWord
  val spacedBWord: Pattern = space >> bWord
  val end: Pattern         = eof

  final object Word {
    def onFirstWord(word: String => AST.Ident): Unit =
      logger.trace_ {
        onFirstWord(word(currentMatch))
      }

    def onFirstWord(ast: AST.Ident): Unit =
      logger.trace {
        result.app(ast)
        state.begin(SEEN_FIRST_WORD)
      }

    def onSpacedWord(word: String => AST.Ident): Unit =
      logger.trace_ {
        onSpacedWord(word(currentMatch.stripLeading()))
      }

    def onSpacedWord(ast: AST.Ident): Unit =
      logger.trace {
        result.app(ast)
      }

    def onNoErrSuffixFirstWord(): Unit =
      logger.trace {
        submit()
      }

    def onNoErrSuffix(): Unit =
      logger.trace {
        onNoErrSuffixFirstWord()
        state.end()
      }

    def onErrSuffixFirstWord(): Unit =
      logger.trace {
        val ast = AST.Invalid.Unrecognized(currentMatch)
        result.app(ast)
      }

    def onErrSuffix(): Unit =
      logger.trace {
        onNoErrSuffixFirstWord()
        state.end()
      }

    def submit(): Unit = logger.trace {}

    val SEEN_FIRST_WORD: State = state.define("Inside Word")
  }

  ROOT                 || aWord       || Word.onFirstWord(AST.Var(_))
  ROOT                 || bWord       || Word.onFirstWord(AST.Var(_))
  ROOT                 || eof         || Word.onNoErrSuffixFirstWord()
  ROOT                 || any         || Word.onErrSuffixFirstWord()
  Word.SEEN_FIRST_WORD || spacedAWord || Word.onSpacedWord(AST.Var(_))
  Word.SEEN_FIRST_WORD || spacedBWord || Word.onSpacedWord(AST.Var(_))
  Word.SEEN_FIRST_WORD || eof         || Word.onNoErrSuffix()
  Word.SEEN_FIRST_WORD || any         || Word.onErrSuffix()

  ////////////////
  //// Result ////
  ////////////////

  override def getResult() =
    result.current.flatMap {
      case AST.Module.any(mod) => Some(mod)
      case a =>
        val line = AST.Block.OptLine(a)
        Some(AST.Module(line, List()))
    }

  final object result {
    var current: Option[AST] = None

    def app(fn: String => AST): Unit =
      app(fn(currentMatch))

    def app(ast: AST): Unit =
      logger.trace {
        current = Some(current match {
          case None    => ast
          case Some(r) => AST.App.Prefix(r, ast)
        })
      }
  }
}

object ParserDef2Small {
  type Result[T] = flexer.Parser.Result[T]
}

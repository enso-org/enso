package org.enso.syntax.text.parser

import org.enso.syntax.text.{xx => bison}
import java.io.Reader

import org.enso.syntax.text.xx.Parser.Lexer._
import org.enso.syntax.text.lexer.Token
import org.enso.syntax.text.lexer.Lexer

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

//////////////////
// Parser Rules //
//////////////////

///////////////
// GenParser //
///////////////

class GenParser(reader: Reader) {
  private val lexer             = new Lexer(reader)
  private val (itokens, tokens) = lexer.lexAll2()

  final def token(t: Int): Rule[Token] =
    Rule((ix) =>
      if (itokens(ix) == t) {
        Some(Result(ix + 1, tokens(ix)))
      } else {
        None
    })

  case class Result[T](offset: Int, value: T) {
    final def map[S](f: T => S): Result[S] = this.copy(value = f(value))
  }

  case class Rule[T](unwrap: (Int) => Option[Result[T]]) {
    final def run(ix: Int): Option[Result[T]] = unwrap(ix)

    final def mapOption[S](f: Option[Result[T]] => Option[Result[S]]): Rule[S] =
      Rule((ix) => f(run(ix)))

    final def mapResult[S](f: Result[T] => Result[S]): Rule[S] =
      mapOption(_.map(f))

    final def map[S](f: T => S): Rule[S] = mapResult(_.map(f))

    final def flatMap[S](f: T => Rule[S]): Rule[S] =
      Rule(ix => {
        run(ix).flatMap(r => f(r.value).run(r.offset))
      })

    final def flatMapResult[S](f: Result[T] => Rule[S]): Rule[S] =
      Rule(ix => {
        run(ix).flatMap(r => f(r).run(r.offset))
      })

    final def next[S](f: Rule[S]): Rule[S] = flatMap(_ => f)

    final def |(that: Rule[T]): Rule[T] = this or that
    final def or(that: Rule[T]): Rule[T] =
      Rule(ix => this.run(ix).orElse(that.run(ix)))

    final def orElse(v: T): Rule[T] = or(Rule(ix => Some(Result(ix, v))))

    @scala.annotation.tailrec
    final def manyWith(ix: Int, f: (T, T) => T, t: Result[T]): Result[T] =
      this.run(ix) match {
        case None    => t
        case Some(r) => manyWith(r.offset, f, r.map(f(t.value, _)))
      }

    final def many1_(): Rule[Unit] = this.flatMap(_ => many_)
    final def many_():  Rule[Unit] = Rule(ix => Some(_many_(ix)))

    @scala.annotation.tailrec
    final def _many_(ix: Int): Result[Unit] =
      this.run(ix) match {
        case None    => Result(ix, Unit)
        case Some(r) => _many_(r.offset)
      }

    final def fold(concat: (T, T) => T): Rule[T] =
      Rule(ix => run(ix).map(r => manyWith(r.offset, concat, r)))
  }

}

////////////
// Parser //
////////////

class Parser(reader: Reader) extends GenParser(reader) {

  final def parse(): Option[Result[AST]] = expr.run(0)

  //  final def expr(): Rule[AST] = vvar.next(vvar).or(vvar.next(cons))

  final def expr(): Rule[AST] = tok.fold(AST.app)

  final def tok():  Rule[AST] = vvar | cons | group | block
  final def vvar(): Rule[AST] = token(VAR).map(AST.fromToken)

  final def cons(): Rule[AST] = token(CONS).map(AST.fromToken)

  //
  final def group(): Rule[AST] =
    token(GROUP_BEGIN).flatMap(
      beg =>
        expr.flatMap(
          exp =>
            token(GROUP_END)
              .map(AST.grouped(beg, exp, _))
              .orElse(AST.grouped(beg, exp))))

  final def block(): Rule[AST] = {
    token(EOL).many1_
      .next(token(BLOCK_BEGIN))
      .mapResult(r => blockLines(r.offset).map(AST.block))
  }

  final def blockLines(ix: Int): Result[Vector[AST]] = {
    expr.run(ix) match {
      case None => Result(ix, Vector[AST]())
      case Some(firstLine) => {
        val lines = Vector.newBuilder[AST]
        lines += firstLine.value
        _blockLines(firstLine.offset, lines)
      }
    }
  }

  @scala.annotation.tailrec
  final def _blockLines(
    startIx: Int,
    lines: mutable.Builder[AST, Vector[AST]]): Result[Vector[AST]] = {
    var ix   = startIx
    var body = true
    token(EOL).many1_.flatMap(_ => expr).run(ix) match {
      case None    => Result(ix, lines.result)
      case Some(r) => _blockLines(r.offset, lines)
    }
  }
}

class BParser(reader: Reader) {
  val lexer  = new Lexer(reader)
  val parser = new bison.Parser(lexer)

  def parse(): Option[AST] = {
    if (parser.parse) {
      Some(parser.result)
    } else {
      None
    }
  }
}

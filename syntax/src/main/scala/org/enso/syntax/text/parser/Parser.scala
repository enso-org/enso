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

case class Rule[T](unwrap: () => Option[T]) {
  final def run():   Option[T] = unwrap()
  final def apply(): Option[T] = run

  final def mapOption[S](f: Option[T] => Option[S]): Rule[S] =
    Rule(() => f(run))

  final def map[S](f: T => S): Rule[S] =
    mapOption(_.map(f))

  final def flatMap[S](f: T => Rule[S]): Rule[S] =
    mapOption(_.flatMap(f(_).run))

  final def |(that: Rule[T]):  Rule[T] = this or that
  final def or(that: Rule[T]): Rule[T] = mapOption(_.orElse(that.run))
  final def default(v: T):     Rule[T] = mapOption(_.orElse(Some(v)))

//  final def foreach(f: T => Unit): Unit = run.foreach(f)

  @scala.annotation.tailrec
  final def manyWith(concat: (T, T) => T, t: T): T = {
    this.run match {
      case None => t
      case Some(t2) => {
        manyWith(concat, concat(t, t2))
      }
    }
  }

  final def many1_(): Rule[Unit] = this.flatMap(_ => many_)
  final def many_():  Rule[Unit] = this.flatMap(_ => many_).default(Unit)

  final def fold(concat: (T, T) => T): Rule[T] =
    mapOption(_.map(manyWith(concat, _)))
}

///////////////
// GenParser //
///////////////

class GenParser(reader: Reader) {
  private val lexer             = new Lexer(reader)
  private val (itokens, tokens) = lexer.lexAll2()
  private var tokenIx: Int      = 0
  var current: Token            = tokens(tokenIx)
  private var icurrent: Int     = itokens(tokenIx)

  final def step(): Unit = {
    if (tokenIx == tokens.size - 1) return
    tokenIx += 1
    current  = tokens(tokenIx)
    icurrent = itokens(tokenIx)
  }

  final def lookahead(shift: Int = 1): Token = tokens(tokenIx + shift)

  final def token(t: Int, name: String = ""): Rule[Token] =
    Rule(() => {
      if (icurrent == t) {
        val result = Some(current)
        step
        println("STEP -> ", current, name)
        result
      } else {
        None
      }
    })

}

////////////
// Parser //
////////////

class Parser(reader: Reader) extends GenParser(reader) {

  final def parse(): Option[AST] = expr.run

  final def expr(): Rule[AST] = tok.fold(AST.app)

  final def tok():  Rule[AST] = vvar | cons | group | block
  final def vvar(): Rule[AST] = token(VAR).map(AST.fromToken)
  final def cons(): Rule[AST] = token(CONS).map(AST.fromToken)

  final def group(): Rule[AST] =
    token(GROUP_BEGIN).flatMap(beg => {
      expr.flatMap(exp => {
        token(GROUP_END)
          .map(AST.grouped(beg, exp, _))
          .default(exp)
      })
    })

  final def block(): Rule[AST] = {
    println(">> 1")
    val out = token(EOL, "n1").many1_
      .flatMap(_ => token(BLOCK_BEGIN))
      .map(beg => {
        val lines = blockLines
        AST.block(lines)
      })
    println("<< 1")
    out
  }

  final def blockLines(): Vector[AST] = {
    println("--- 1")
    println(current)
    expr.run match {
      case None => {
        println("--=--")
        Vector[AST]()
      }
      case Some(firstLine) => {
        println("--- 2")
        println(current)
        println(firstLine)
        val lines = Vector.newBuilder[AST]
        lines += firstLine
        blockLines_(lines)
      }
    }
  }

  final def blockLines_(
    lines: mutable.Builder[AST, Vector[AST]]): Vector[AST] = {
    var body = true
    println("--- 3")
    println(current)
    while (body) {
      println(">> 2")
      token(EOL, "n2").many1_.flatMap(_ => expr).run match {
        case None    => body = false
        case Some(e) => lines += e
      }
      println("<< 2")
    }
    lines.result
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

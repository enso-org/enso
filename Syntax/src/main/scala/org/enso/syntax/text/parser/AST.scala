package org.enso.syntax.text.parser

import org.enso.syntax.text.lexer.Token
import org.enso.syntax.text.{lexer => token}

/////////
// AST //
/////////

case class AST(offset: Int, span: Int, symbol: Symbol)

// class Sym[T](offset:Int, span:Int, element:T)

////////////
// Symbol //
////////////

trait Symbol

case object NONE extends Symbol

// Identifiers
case class Var(name: String)        extends Symbol
case class Cons(name: String)       extends Symbol
case class Operator(name: String)   extends Symbol
case class App(func: AST, arg: AST) extends Symbol
case class Block(body: Vector[AST]) extends Symbol
case class Grouped(body: AST)       extends Symbol

//

object AST {

  def fromToken(tok: Token): AST = {
    tok.symbol match {
      case token.Var(name)  => AST(tok.offset, tok.span, Var(name))
      case token.Cons(name) => AST(tok.offset, tok.span, Cons(name))
    }
  }

  def app(fn: AST, arg: AST): AST = {
    AST(fn.offset, fn.span + arg.span, App(fn.copy(offset = 0), arg))
  }

  def emptyBlock(): AST = {
    AST(0, 0, Block(Vector()))
  }

  def block(lines: Vector[AST]): AST = {
    AST(0, 0, Block(lines))
  }

  def grouped(begin: Token, body: AST, end: Token): AST = {
    val offset = begin.offset
    val span   = begin.span + body.offset + body.span + end.offset + end.span
    AST(offset, span, Grouped(body))
  }

  // FIXME - should report error about lack of closing paren
  def grouped(begin: Token, body: AST): AST = {
    val offset = begin.offset
    val span   = begin.span + body.offset + body.span // + end.offset + end.span
    AST(offset, span, Grouped(body))
  }

}

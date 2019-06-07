package org.enso.syntax.text.parser

import org.enso.syntax.text.lexer.Token
import org.enso.syntax.text.{lexer => token}


/////////
// AST //
/////////

case class AST (offset:Int, span:Int, symbol:Symbol) 



////////////
// Symbol //
////////////

abstract class Symbol 
case object NONE                           extends Symbol

// Identifiers
case class Var      (name:String)          extends Symbol
case class Operator (name:String)          extends Symbol
case class App      (func:AST, arg:AST)    extends Symbol
case class Block    (body:Vector[AST])     extends Symbol




//


object AST {
  def fromToken(tok:Token):AST = {
    tok.symbol match {
      case token.Var(name) => AST(0,0,Var(name))
    }
  }
  def app(fn:AST, arg:AST):AST = {
    AST(fn.offset,fn.span + arg.span,App(fn.copy(offset=0),arg))
  }
  def emptyBlock():AST = {
    AST(0,0,Block(Vector()))
  }
}
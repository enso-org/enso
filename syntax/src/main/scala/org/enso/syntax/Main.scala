
package org.enso.syntax

// import org.enso.syntax.text.parser.{Parser}
import java.io.{Reader, StringReader}

import org.enso.syntax.text.lexer.{Scanner, EOF, Token}
import org.enso.syntax.text.xx.Parser
import org.enso.syntax.text.xx.Parser.Lexer._
// import org.enso.syntax.text.{parser => AST}
import org.enso.syntax.text.parser.AST

class SS(scanner:Scanner) extends Parser.Lexer {
  private var _done = false

  def getLVal():Token = {
    scanner.value
  }

  def yyerror(s:String) {
    println("!!! ERROR !!!")
    println(s)
  }

  def yylex():Int = {
    scanner.lex
  }

  def lex(): Token = {
    val tok = yylex
    if (tok == -1) {
      _done = true;
      return Token(EOF,0,0)
    }
    return getLVal
  }

  def lexAll(): Vector[Token] = {
    var builder = Vector.newBuilder[Token]
    do {
      builder += lex
    } while (!_done)
    builder.result
  }

  def lexAll2(): (Vector[Int],Vector[Token]) = {
    var builder_t = Vector.newBuilder[Int]
    var builder   = Vector.newBuilder[Token]
    do {
      val tok  = yylex
      var tval = getLVal
      if (tok == -1) {
        tval  = Token(EOF,0,0)
        _done = true
      }
      builder_t += tok
      builder   += tval 
    } while (!_done)
    (builder_t.result, builder.result)
  }

}


class PP(reader:Reader) {
  val lexer = new SS(new Scanner(reader))


  //////////////////////
  // Token Management //
  //////////////////////

  val (itokens, tokens)  = lexer.lexAll2()
  var tokenIx = 0
  var current  : Token = tokens(tokenIx)
  var icurrent : Int   = itokens(tokenIx)

  def step(): Unit = {
    if (tokenIx == tokens.size - 1) {
      return 
    }
    tokenIx += 1
    current = tokens(tokenIx)
    icurrent = itokens(tokenIx)
  }





  def parse():Option[AST] = {
    manyWith(AST.app,()=>tok)
  }


  def tok():Option[AST] = {
    token(VAR).map(AST.fromToken)
  }

  // def block




  def or[T](l:()=>Option[T],r:()=>Option[T]) {
    l() match {
      case Some(a) => Some(a)
      case None    => r()
    }
  }

  def token(t:Int):Option[Token] = {
    if(icurrent==t) {
      val result = Some(current)
      step
      result
    } else {
      None
    }
  }



  def manyWith(concat:(AST,AST)=>AST,f:()=>Option[AST]): Option[AST] = {
    f() match {
      case None => None
      case Some(ast) => {
        Some(manyWith_(concat,f,ast))
      }
    }
  }

  def manyWith_(concat:(AST,AST)=>AST,f:()=>Option[AST],ast:AST): AST = {
    f() match {
      case None => ast
      case Some(ast2) => {
        manyWith_(concat,f,concat(ast,ast2))
      }
    }
  }
}




object Main extends App {
  val str     = "a b"
  val reader  = new StringReader(str)
  val reader2 = new StringReader(str)
  val scanner  = new Scanner(reader)
  val scanner2 = new Scanner(reader2)
  val ss      = new SS(scanner)
  val ss2     = new SS(scanner2)
  val parser = new Parser(ss)

  val pp = new PP(new StringReader(str))
  pprint.pprintln(ss2.lexAll)
  pprint.pprintln(pp.parse)
  pprint.pprintln(parser.parse)
  // val parser = new Parser(reader)
  // val ast = parser.parse
  pprint.pprintln(parser.result,width=3,height=1000)
}
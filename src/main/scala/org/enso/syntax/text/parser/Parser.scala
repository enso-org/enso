package org.enso.syntax.text.parser

import java.io.{Reader}
import org.enso.syntax.text.lexer.{Lexer, Token}
import org.enso.syntax.text.{lexer => token}
import scala.collection.immutable.{Vector}
import scala.collection.mutable.{Builder}

class Parser(reader:Reader) {
  val lexer = new Lexer(reader)


  //////////////////////
  // Token Management //
  //////////////////////

  val tokens  = lexer.lexAll()
  var tokenIx = 0
  var current : Token = tokens(tokenIx)

  def step(): Token = {
    if (tokenIx == tokens.size - 1) {
      return Token(token.EOF,0,0)
    }
    tokenIx += 1
    current = tokens(tokenIx)
    if (current.symbol == token.EOL) {
      line  += 1
      column = 0
    } else {
      column += current.offset + current.span
    }
    current
  }

  def lookup(i:Int=1): Token = {
    val ix = tokenIx + i
    if (ix >= 0 && ix < tokens.size) {
      tokens(ix)
    } else {
      Token(token.EOF,0,0)
    }
  }

  def next(): Token = {
    lookup()
  }
  
  def previous(): Token = {
    lookup(-1)
  }


  // var indents : Stack[Int] = new Stack()
  // indents.push(0)

  // def indent(): Int = {
  //   indents.head
  // }

  var column : Int = 0
  var line   : Int = 0
  var indent : Int = 0

  def withIndent[T](newIndent:Int,f:()=>T):T = {
    val oldIndent = indent
    indent = newIndent
    val result = f()
    indent = oldIndent
    result
  }



  def parse(): AST = {
    expr() match {
      case Some(ast) => ast
      case None => AST(0,0,NONE)
    }
  }

  def expr(): Option[AST] = {
    manyWith(app, patternToken)
    // .flatMap(pat => {
    //   if(current.symbol == token.EOL && next.offset > indent) {
    //     step
    //     withIndent(next.offset, () => Some(app(pat,block)))
    //   } else {
    //     Some(pat)
    //   }
    // })
  }

  def block(): AST = {
    AST(0,0,Block(many(() => blockLine)))
  }

  def blockLine(): Option[AST] = {
    if(next.offset == indent) {
      val out = expr
      println("!!!!!--")
      println(out)
      out
    } else {
      None
    }
  }


  // def parseExprX(): AST = {
  //   current.symbol match {
  //     case token.Var(name) => {
  //       AST(Var(name),current.offset,current.span)
  //     }
  //     case x => {
  //       println("!!!")
  //       println(x)
  //       AST(NONE,0,0)
  //     }
  //   }
  // }

  def patternToken(tok:Token): Option[AST] = {
    tok.symbol match {
      case token.Var      (name) => Some(AST(tok.offset,tok.span, Var (name)))
      case token.Operator (name) => Some(AST(current.offset,current.span, Operator (name)))
      case token.EOL             => {
        if (next.offset > indent) {
          step
          withIndent(next.offset, () => Some(block))
        } else {
          None
        }
      }
      case _ => None
    }
  }

  def many(f:()=>Option[AST]): Vector[AST] = {
    f() match {
      case None => Vector()
      case Some(ast) => {
        step
        val builder = Vector.newBuilder[AST]
        builder += ast
        many_(f,builder)
        builder.result
      }
    }
  }

  def many_(f:()=>Option[AST], builder:Builder[AST,Vector[AST]]): Unit = {
    f() match {
      case None => return
      case Some(ast) => {
        builder += ast
        many_(f,builder)
      }
    }
  }

  def manyWith(concat:(AST,AST)=>AST,f:(Token)=>Option[AST]): Option[AST] = {
    f(current) match {
      case None => None
      case Some(ast) => {
        step
        Some(manyWith_(concat,f,ast))
      }
    }
  }

  def manyWith_(concat:(AST,AST)=>AST,f:(Token)=>Option[AST],ast:AST): AST = {
    f(current) match {
      case None => ast
      case Some(ast2) => {
        step
        manyWith_(concat,f,concat(ast,ast2))
      }
    }
  }
  

  def app(func:AST, arg:AST): AST = {
    AST(func.offset,func.span + arg.span,App(func.copy(offset=0),arg))
  }
}

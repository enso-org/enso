package org.enso.syntax.text.lexer

import java.io.StringReader
import java.io.Reader
import scala.collection.immutable.Vector
import org.enso.syntax.text.xx.Parser

class Lexer(reader: Reader) extends Scanner(reader) with Parser.Lexer {
  private var _done = false

  def getLVal(): Token = {
    value
  }

  def yyerror(s: String) {
    println("!!! ERROR !!!")
    println(s)
  }

  def lexTok(): Token = {
    val tok = yylex
    if (tok == -1) {
      _done = true;
      return Token(EOF, 0, 0)
    }
    return getLVal
  }

  def lexAll(): Vector[Token] = {
    var builder = Vector.newBuilder[Token]
    do {
      builder += lexTok
    } while (!_done)
    builder.result
  }

  def lexAll2(): (Vector[Int], Vector[Token]) = {
    var builder_t = Vector.newBuilder[Int]
    var builder   = Vector.newBuilder[Token]
    do {
      val tok  = yylex
      var tval = getLVal
      if (tok == -1) {
        tval  = Token(EOF, 0, 0)
        _done = true
      }
      builder_t += tok
      builder += tval
    } while (!_done)
    (builder_t.result, builder.result)
  }

}
// class Lexer (reader:Reader) {
//   val scanner       = new Scanner(reader)
//   private var _done = false

//   def this(str:String) {
//     this(new StringReader(str))
//   }

//   def lex(): Token = {
//     if (done) {
//       return Token(EOF,0,0)
//     }
//     if (scanner.done) {
//       _done = true
//       return lex
//     }
//     val token = scanner.lex
//     if (token == null) {
//       _done = true
//       return lex
//     } else {
//       return token
//     }
//   }

//   def lexAll(): Vector[Token] = {
//     var builder = Vector.newBuilder[Token]
//     do {
//       builder += lex
//     } while (!done)
//     builder.result
//   }

//   def done(): Boolean = {
//     return _done;
//   }
// }

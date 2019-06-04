package org.enso.syntax.text.lexer

import java.io.{StringReader, Reader}

class Lexer (reader:Reader) {
  val scanner       = new Scanner(reader)
  private var _done = false

  def this(str:String) {
    this(new StringReader(str))
  }

  def lex(): Token = {
    if (done) {
      return Token(EOF,0,0)
    }
    if (scanner.done) {
      _done = true
      return lex
    }
    val token = scanner.lex
    if (token == null) {
      _done = true
      return lex
    } else {
      return token
    }
  }

  def lexAll(): List[Token] = {
    var lst = List[Token]()
    do {
      lst = lex +: lst
    } while (!done)
    lst.reverse
  }

  def done(): Boolean = {
    return _done;
  }
}
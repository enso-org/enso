package org.enso.syntax.text.lexer

import java.io.{StringReader, StringWriter}
import collection.mutable.Stack
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {

  def lex (input:String): List[Token] = {
    var lst     = List[Token]()
    val reader  = new StringReader(input)
    val scanner = new Lexer(reader)
    do {
      val token = scanner.lex
      if (token != null) {
        lst = token +: lst
      }
    } while (!scanner.done())
    lst.reverse
  }

  def lex_ (input:String): List[Symbol] = {
    lex(input).map(tok => tok.symbol)
  }

  def assertLex (input:String, result:List[Symbol]) = {
    assert(lex_(input) == result)
  }

  def check (input:String, result:List[Symbol]) = {
    it should ("lex '" + input + "'") in { assertLex (input, result) }
  }

  def unexpectedSuffix (input:String): Symbol = {
    Invalid(UnexpectedSuffix(input))
  }

  // Identifiers
  check("_"      , Wildcard      :: Nil)
  check("Name"   , Cons("Name")  :: Nil)
  check("name"   , Var("name")   :: Nil)
  check("name'"  , Var("name'")  :: Nil)
  check("name''" , Var("name''") :: Nil)
  check("name'a" , Var("name'")  :: unexpectedSuffix("a") :: Nil)
  check("name_"  , Var("name_")  :: Nil)
  check("name_'" , Var("name_'") :: Nil)
  check("name'_" , Var("name'")  :: unexpectedSuffix("_") :: Nil)
  check("name`"  , Var("name")   :: unexpectedSuffix("`") :: Nil)
  
  // Operators
  check("="    , Operator("=")   :: Nil)
  check("=="   , Operator("==")  :: Nil)
  check("==="  , Operator("==")  :: unexpectedSuffix("=") :: Nil)
  check(","    , Operator(",")   :: Nil)
  check("."    , Operator(".")   :: Nil)
  check(".."   , Operator("..")  :: Nil)
  check("..."  , Operator("...") :: Nil)
  check("...." , Operator("...") :: unexpectedSuffix(".") :: Nil)
  check(">="   , Operator(">=")  :: Nil)
  check("<="   , Operator("<=")  :: Nil)
  check("/="   , Operator("/=")  :: Nil)
  check("+="   , Modifier("+=")  :: Nil)
  check("-="   , Modifier("-=")  :: Nil)
  check("-=-"  , Modifier("-=")  :: unexpectedSuffix("-") :: Nil)

  // Layouts
  check("(a)" , GroupBegin  :: Var("a") :: GroupEnd  :: Nil)
  check("[a]" , ListBegin   :: Var("a") :: ListEnd   :: Nil)
  check("{a}" , RecordBegin :: Var("a") :: RecordEnd :: Nil)
  
}
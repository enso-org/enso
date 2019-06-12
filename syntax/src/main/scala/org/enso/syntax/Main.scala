package org.enso.syntax

import java.io.Reader
import java.io.StringReader

import org.enso.syntax.text.lexer.Lexer
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser

import scala.language.implicitConversions

import org.enso.syntax.text.lexer.SParser

object Main extends App {

  var indent = 0

  def pprint(s: String) {
    print("  " * indent)
    val (l, r2) = s.span(x => x != '(' && x != ')')
    print(l)
    if (r2 == "") {
      println
      return
    }

    val (m, r) = r2.splitAt(1)

    if (m == "(") {
      indent += 1
      println(m)
      pprint(r)
    } else if (m == ")") {
      indent -= 1
      println(m)
      pprint(r)
    }

  }

//  val str = "a (b"
  val str = "a\n b\n c" // .stripMargin
  println(str)
  val reader = new StringReader(str)
  val ss     = new Lexer(reader)
  val toks   = ss.lexAll()
  pprint(toks.toString)

  val sparser = new SParser(new StringReader(str))

  val bparser = new BParser(new StringReader(str))
  val parser  = new Parser(new StringReader(str))

  pprint(bparser.parse.toString())
  pprint(parser.parse.toString())
  pprint("!")
  println(sparser.strInput)
  pprint(sparser.parse.toString)
}

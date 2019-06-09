package org.enso.syntax

import java.io.StringReader

import org.enso.syntax.text.lexer.Lexer
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser

import scala.language.implicitConversions

object Main extends App {

  var indent = 0

  def pprint(s: String) {
    print("  " * indent)
    val (l, r2) = s.span((x) => (x != '(' && x != ')'))
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
  val str =
    """|a
       |      
       |  a
       |  c""".stripMargin
  println(str)
  val reader = new StringReader(str)
  val ss     = new Lexer(reader)
  pprint(ss.lexAll.toString())

  val bparser = new BParser(new StringReader(str))
  val parser  = new Parser(new StringReader(str))

  pprint(bparser.parse.toString())
  pprint(parser.parse.toString())
  pprint("!")
}

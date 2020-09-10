package org.enso.parser

import org.enso.ast.Ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class ParserTest extends AnyFlatSpec with Matchers {
  val parser: Parser = Parser()

  it should "parse file" in {
    val expected = Ast.Ast(uid=None, len=0, off=0, ast=Ast.Txt.Text("Hello!"))
    expected shouldBe parser.parseStr("Hello!")
    expected shouldBe parser.parseFile("Hello!")
    expected shouldBe parser.lexStr("Hello!")
    expected shouldBe parser.lexFile("Hello!")
    parser.bench().ast shouldBe an[Ast.App.Prefix]
  }
}

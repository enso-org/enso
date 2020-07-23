package org.enso.syntax.text

import org.enso.flexer.Reader
import org.enso.syntax.text.AST._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFlatSpec with Matchers {

  def assertSpan(input: String, ast: AST): Assertion = {
    val gotSpan      = ast.span
    val expectedSpan = new Reader(input).toString().length
    gotSpan shouldEqual expectedSpan
  }

  def assertModule(input: String, result: AST): Assertion = {
    val parser = Parser()
    val module = parser.run(input)
    assertSpan(input, module)
    val rmodule = parser.dropMacroMeta(module)
    assert(rmodule == result)
    assert(module.show() == new Reader(input).toString())
  }

  def assertExpr(input: String, result: AST): Assertion = {
    val parser = Parser()
    val module = parser.run(input)
    assertSpan(input, module)
    val rmodule = parser.dropMacroMeta(module)
    val tail    = module.lines.tail
    if (!tail.forall(_.elem.isEmpty)) fail("Multi-line block")
    else {
      rmodule.lines.head.elem match {
        case None => fail("Empty expression")
        case Some(e) =>
          assert(e == result)
          assert(module.show() == new Reader(input).toString())
      }
    }
  }

  def assertIdentity(input: String): Assertion = {
    val module = Parser().run(input)
    val idmap1 = module.idMap
    val idmap2 = Parser().run(new Reader(input), idmap1).idMap
    assertSpan(input, module)
    assert(module.show() == new Reader(input).toString())
    assert(idmap1 == idmap2)
  }

  implicit class TestString(input: String) {
    def parseTitle(str: String): String = {
      val maxChars = 20
      val escape   = (str: String) => str.replace("\n", "\\n")
      val str2     = escape(str)
      val str3 =
        if (str2.length < maxChars) str2
        else str2.take(maxChars) + "..."
      s"parse `$str3`"
    }

    private val testBase = it should parseTitle(input)

    def ?=(out: AST) = testBase in { assertExpr(input, out) }
    def ??=(out: Module) = testBase in { assertModule(input, out) }
    def testIdentity()   = testBase in { assertIdentity(input)    }
  }

  "Thingy" should "thingy" in {
    val input = "aaaaa"
    val parser = Parser()
    val result = parser.run(input)
    val dropped = parser.dropMacroMeta(result)

    println(dropped)
  }

  "Test" should "test" in {
    val input = "aaaaa bb a a bbbbb a bbbb"
    val parser = Parser()
    val result = parser.run(input)
    val dropped = parser.dropMacroMeta(result)

    println(dropped)
  }

  "Thing" should "fail" in {
    val input = "aaaa c"
    val parser = Parser()
    val result = parser.run(input)
    val dropped = parser.dropMacroMeta(result)

    println(dropped)
  }
}

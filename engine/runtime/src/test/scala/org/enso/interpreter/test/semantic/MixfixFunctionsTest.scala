package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class MixfixFunctionsTest extends InterpreterTest {
  val subject = "Mixfix functions"

  subject should "be able to be defined as a method" in {
    val code =
      """
        |type Foo a
        |
        |Foo.if_then = x -> case this of
        |  Foo a -> a + x
        |
        |main = if Foo 2 then 8
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  subject should "easily support multiple arguments" in {
    val code =
      """
        |type Foo a b
        |
        |Foo.if_then_else = a b -> case this of
        |  Foo x y -> x + y + a + b
        |
        |main = if (Foo 1 2) then 3 else 4
        |""".stripMargin

    eval(code) shouldEqual 10
  }
}

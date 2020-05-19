package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class FunctionSugarTest extends InterpreterTest {

  "Sugared function definitions" should "work" in {
    val code =
      """
        |main =
        |    f a b = a - b
        |    f 10 20
        |""".stripMargin

    eval(code) shouldEqual -10
  }

  "Sugared method definitions" should "work" in {
    val code =
      """
        |Unit.foo a b = a * b - a
        |
        |main = Unit.foo 2 3
        |""".stripMargin

    eval(code) shouldEqual 4
  }
}

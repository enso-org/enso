package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class FunctionSugarTest extends InterpreterTest {
  override def subject: String = "Function Definition Sugar"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "work for local functions" in {
      val code =
        """
          |main =
          |    f a b = a - b
          |    f 10 20
          |""".stripMargin

      eval(code) shouldEqual -10
    }

    "work for methods" in {
      val code =
        """from Builtins import all
          |
          |Unit.foo a b = a * b - a
          |
          |main = Unit.foo 2 3
          |""".stripMargin

      eval(code) shouldEqual 4
    }
  }
}

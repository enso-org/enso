package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}
import org.enso.testkit.ReportLogsOnFailure

class FunctionSugarTest extends InterpreterTest with ReportLogsOnFailure {
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
        """import Standard.Base.Nothing
          |
          |Nothing.foo a b = a * b - a
          |
          |main = Nothing.foo 2 3
          |""".stripMargin

      eval(code) shouldEqual 4
    }
  }
}

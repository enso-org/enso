package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class OperatorSectionsTest extends InterpreterTest {
  override def subject: String = "Operator Sections"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "work when left" in {
      val code =
        """
          |main =
          |    f = (1 +)
          |    f 9
          |""".stripMargin

      eval(code) shouldEqual 10
    }

    "work when two-sided" in {
      val code =
        """
          |main =
          |    f = (-)
          |    f 1 6
          |""".stripMargin

      eval(code) shouldEqual -5
    }

    "work when right" in {
      val code =
        """
          |main =
          |    f = (/ 10)
          |    f 20
          |""".stripMargin

      eval(code) shouldEqual 2
    }
  }
}

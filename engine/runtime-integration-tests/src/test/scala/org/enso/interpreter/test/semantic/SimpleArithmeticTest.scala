package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class SimpleArithmeticTest extends InterpreterTest {

  override def subject: String = "Arithmetic"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "1 = 1" in {
      eval("main = 1") shouldEqual 1
    }

    "1 + 1 = 2" in {
      eval("import Standard.Base.Data.Numbers\nmain = 1 + 1") shouldEqual 2
    }

    "2 + (2 * 2) = 6" in {
      eval(
        "import Standard.Base.Data.Numbers\nmain = 2 + (2 * 2)"
      ) shouldEqual 6
    }

    "2 + 2 * 3 = 8" in {
      eval("import Standard.Base.Data.Numbers\nmain = 2 + 2 * 3") shouldEqual 8
    }

    "2 * 2 / 2 = 2" in {
      eval("import Standard.Base.Data.Numbers\nmain = 2 * 2 / 2") shouldEqual 2
    }

    "-1 = -1" in {
      eval("main = -1") shouldEqual -1
    }

    "1 + -1 = 0" in {
      eval("import Standard.Base.Data.Numbers\nmain = 1 + -1") shouldEqual 0
    }

    "negate expressions" in {
      val code =
        """import Standard.Base.Data.Numbers
          |main =
          |    expr = (10 * 3+2) - 1
          |    -expr
          |""".stripMargin

      eval(code) shouldEqual -49
    }
  }
}

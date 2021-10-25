package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class CommentsTest extends InterpreterTest {
  override def subject: String = "All Comments"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "be ignored in execution" in {
      val code =
        """
          |## Documented
          |   Thoroughly.
          |main =
          |    # commented out line
          |    x = 5 + 7 # commented out piece
          |    y = 564
          |    x + y
          |""".stripMargin
      eval(code) shouldEqual 576
    }
  }
}

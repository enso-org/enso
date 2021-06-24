package org.enso.std.test

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class NumberTest extends InterpreterTest {
  override def subject = "Numbers"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "support equality comparisons" in {
      val code =
        """from Standard.Builtins import all
          |
          |main =
          |    IO.println 7==5
          |    IO.println 30==30
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("False", "True")
    }

    "support a recursive sum case" in {
      val code =
        """
          |main = sumTo ->
          |    summator = acc -> current ->
          |        if current == 0 then acc else summator acc+current current-1
          |
          |    res = summator 0 sumTo
          |    res
          |""".stripMargin
      eval(code).call(100) shouldEqual 5050
    }
  }
}

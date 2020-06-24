package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class InteropTest extends InterpreterTest {
  override def subject: String = "Interop Library"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {
    "support tail recursive functions" in {
      val code =
        """
          |main =
          |    recurFun = i -> if i == 0 then 0 else recurFun i-1
          |    recurFun
          |""".stripMargin

      val recurFun = eval(code)
      recurFun.call(15) shouldEqual 0
    }

    "support calling curried functions" in {
      val code =
        """
          |main =
          |    fun = x -> y -> z -> x + y + z
          |    fun y=1
          |""".stripMargin

      val curriedFun = eval(code)
      curriedFun.call(2, 3) shouldEqual 6
    }

    "support creating curried calls" in {
      val code =
        """
          |main = x -> y -> z -> x + y + z
          |""".stripMargin

      val fun = eval(code)
      fun.call(1).call(2).call(3) shouldEqual 6
    }

    "work with oversaturated calls on unresolved methods returned from functions" in {
      val code =
        """
          |Any.method = this
          |
          |main = x -> method
          |""".stripMargin

      val fun = eval(code)
      fun.call(1, 2) shouldEqual 2
    }
  }
}

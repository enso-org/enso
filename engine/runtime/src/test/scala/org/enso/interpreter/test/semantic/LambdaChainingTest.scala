package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class LambdaChainingTest extends InterpreterTest {

  override def subject: String = "Chains of Lambdas"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "evaluate as expected" in {
      val code =
        """
          |main =
          |    fn = a -> b -> c -> a + b + c
          |    fn 1 2 3
          |""".stripMargin

      eval(code) shouldEqual 6
    }

    "evaluate as expected with shadowed parameters" in {
      val code =
        """
          |main =
          |    fn = a -> b -> a -> a + b
          |    fn 1 2 3
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "evaluate as expected with defaults" in {
      val code =
        """
          |main =
          |    fn = a -> (b = a) -> (c = b + 1) -> b + c
          |    fn 3
          |""".stripMargin

      eval(code) shouldEqual 7
    }

    "evaluate as expected with default and shadowed parameters" in {
      val code =
        """
          |main =
          |    fn = a -> (b = a) -> (a = b + 1) -> a + b
          |    fn 3
          |""".stripMargin

      eval(code) shouldEqual 7
    }

    "work properly with lazy parameters" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    fn = a -> ~b -> ~c ->
          |        b
          |        a
          |
          |    fn 10 (IO.println 10) (IO.println 20)
          |""".stripMargin

      eval(code) shouldEqual 10
      consumeOut shouldEqual List("10")
    }

    "work properly with complex shadowing" in {
      val code =
        """
          |main =
          |    fn = x -> (y = x) -> (x = x + 1) -> x + y
          |    fn 1
          |""".stripMargin

      eval(code) shouldEqual 3
    }
  }
}

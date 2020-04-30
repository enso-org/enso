package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class LambdaChainingTest extends InterpreterTest {

  "Chains of lambdas" should "evaluate as expected" in {
    val code =
      """
        |main =
        |    fn = a -> b -> c -> a + b + c
        |    fn 1 2 3
        |""".stripMargin

    eval(code) shouldEqual 6
  }

  "Chains of lambdas with shadowed parameters" should "evaluate as expected" in {
    val code =
    """
        |main =
        |    fn = a -> b -> a -> a + b
        |    fn 1 2 3
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  "Chains of lambdas with defaults" should "evaluate as expected" in {
    val code =
      """
        |main =
        |    fn = a -> (b = a) -> (c = b + 1) -> b + c
        |    fn 3
        |""".stripMargin

    eval(code) shouldEqual 7
  }

  "Chains of lambdas with defaults and shadowed parameters" should "evaluate as expected" in {
    val code =
      """
        |main =
        |    fn = a -> (b = a) -> (a = b + 1) -> a + b
        |    fn 3
        |""".stripMargin

    eval(code) shouldEqual 7
  }

  "Chains of lambdas with lazy parameters" should "work properly" in {
    val code =
      """
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

  "Chains of lambdas with complex shadowing" should "work properly" in {
    val code =
      """
        |main =
        |    fn = x -> (y = x) -> (x = x + 1) -> x + y
        |    fn 1
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Chains of lambdas" should "work syntactically" in {
    val code =
      """
        |main =
        |    fn1 = a -> b=a -> a + b
        |    fn2 = a -> (b = a) -> (a = b + 1) -> a + b
        |
        |    fn1 1 2
        |""".stripMargin

    pending
    eval(code) shouldEqual 3
  }
}

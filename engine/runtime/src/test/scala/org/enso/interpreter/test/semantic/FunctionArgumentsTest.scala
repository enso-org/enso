package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class FunctionArgumentsTest extends InterpreterTest {
  "Functions" should "take arguments and use them in their bodies" in {
    val code =
      """
        |x -> x * x
        |""".stripMargin

    val function = eval(code)
    function.call(1) shouldEqual 1
    function.call(4) shouldEqual 16
  }

  "Function arguments from outer scope" should "be visible in the inner scope" in {
    val code =
      """
        |a ->
        |  add = a b -> a + b
        |  adder = b -> add a b
        |  adder 2
      """.stripMargin

    eval(code).call(3) shouldEqual 5
  }

  "Lambdas" should "be callable directly without assignment" in {
    val code =
      """
        |(x y -> x * y) 5 6
        |""".stripMargin
    eval(code) shouldEqual 30
  }

  "Recursion" should "work" in {
    val code =
      """
        |sumTo = x -> ifZero x 0 (x + (sumTo (x-1)))
        |sumTo 10
      """.stripMargin

    eval(code) shouldEqual 55
  }

  "Function calls" should "accept more arguments than needed and pass them to the result upon execution" in {
    val code =
      """
        |f = x -> z -> x + z
        |f 1 2
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Function calls" should "allow oversaturation and execute until completion" in {
    val code =
      """
        |f = x y -> w -> z -> x * y + w + z
        |f 3 3 10 1
        |""".stripMargin

    eval(code) shouldEqual 20
  }

  "Function calls" should "be able to return atoms that are evaluated with oversaturated args" in {
    val code =
      """
        |f = x -> Cons
        |
        |myCons = f 1 2 3
        |
        |case myCons of
        |  Cons h t -> h + t
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  "Methods" should "support the use of oversaturated args" in {
    val code =
      """
        |Unit.myMethod = 1
        |
        |f = x -> myMethod
        |t = f 10 Unit
        |t
        |""".stripMargin

    eval(code) shouldEqual 1
  }

  "Recursion closing over lexical scope" should "work properly" in {
    val code =
      """
        |summator = current ->
        |  ifZero current 0 ((x -> summator (current - 1)) 0)
        |res = summator 0
        |res
        |""".stripMargin

    eval(code) shouldEqual 0
  }
}

package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class GroupingTest extends InterpreterTest {
  val condition = "be able to be grouped"

  "Arbitrary lambdas" should condition in {
    val code =
      """
        |main = (x -> x)
        |""".stripMargin

    eval(code).call(5) shouldEqual 5
  }

  "RHS of an assignment" should condition in {
    val code =
      """
        |main =
        |    fn = (x -> x)
        |    fn 10
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  "Forced terms and lazy arguments" should condition in {
    val code =
      """
        |main =
        |    ifTest = c -> (~ifT) -> ~ifF -> ifZero c ifT ifF
        |    sum = c -> acc -> ifTest c acc (sum c-1 acc+c)
        |    sum 10000 0
        |""".stripMargin

    eval(code) shouldEqual 50005000
  }

  "Arbitrary arguments" should condition in {
    val code =
      """
        |main = (x) -> x
        |""".stripMargin

    eval(code).call(5) shouldEqual 5
  }

  "Pattern matches" should condition in {
    val code =
      """
        |main =
        |    fn = x -> case x of
        |        (Cons h t) -> h + fn t
        |        (_) -> 0
        |
        |    fn (Cons 7 Nil)
        |""".stripMargin

    eval(code) shouldEqual 7
  }

  "Method calls" should condition in {
    val code =
      """
        |type Foo
        |Foo.bar = number -> number + 1
        |
        |main = (Foo.bar) 10
        |""".stripMargin

    eval(code) shouldEqual 11
  }
}

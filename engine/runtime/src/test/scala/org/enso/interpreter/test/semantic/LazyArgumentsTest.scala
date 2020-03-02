package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class LazyArgumentsTest extends InterpreterTest {
  val subject = "Lazy arguments"

  subject should "work in basic expressions" in {
    val code =
      """
        |main =
        |    lazyId = ~x -> ~x
        |    lazyId (1 + 1)
        |""".stripMargin

    eval(code) shouldEqual 2
  }

  subject should "not get executed upfront" in {
    val code =
      """
        |main =
        |    foo = i ~x ~y -> ifZero i ~x ~y
        |    foo 1 (IO.println 1) (IO.println 2)
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("2")
  }

  subject should "work well with tail recursion" in {
    val code =
      """
        |main =
        |    ifTest = c ~ifT ~ifF -> ifZero c ~ifT ~ifF
        |    sum = c acc -> ifTest c acc (sum c-1 acc+c)
        |    sum 10000 0
        |""".stripMargin
    eval(code) shouldEqual 50005000
  }

  subject should "work in non-tail positions" in {
    val code =
      """
        |main =
        |    suspInc = ~x -> 1 + ~x
        |    suspInc (suspInc 10)
        |""".stripMargin

    eval(code) shouldEqual 12
  }

  subject should "work properly with method dispatch" in {
    val code =
      """
        |type Foo
        |type Bar
        |
        |Foo.method = ~x -> 10
        |Bar.method = x -> 10
        |
        |main =
        |    Foo.method (IO.println 1)
        |    Bar.method (IO.println 2)
        |    Foo.method (IO.println 3)
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("2")
  }

  subject should "work properly with oversaturated arguments" in {
    val code =
      """
        |main =
        |    ifTest = c ~ifT ~ifF -> ifZero c ~ifT ~ifF
        |    foo = c -> ifTest c
        |
        |    foo 0 (IO.println 1) (IO.println 2)
        |    foo 1 (IO.println 3) (IO.println 4)
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("1", "4")
  }

  subject should "work properly with defaulted arguments" in {
    val code =
      """
        |main = a (~b = Panic.throw 1) -> a
        |""".stripMargin
    eval(code).call(1) shouldEqual 1
  }
}

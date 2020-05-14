package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class LambdaShorthandArgsTest extends InterpreterTest {

  val subject = "Lambda shorthand arguments"

  subject should "work for simple applications" in {
    val code =
      """
        |main =
        |    f = a -> b -> c -> a + b - c
        |    g = f _ 5 5
        |    g 10
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  subject should "work for named applications" in {
    val code =
      """
        |main =
        |    f = a -> b -> a - b
        |    g = f (b = _)
        |    g 10 5
        |""".stripMargin

    eval(code) shouldEqual -5
  }

  subject should "work for functions in applications" in {
    val code =
      """
        |main =
        |    add = a -> b -> a + b
        |    sub = a -> b -> a - b
        |    f = _ 10 5
        |    res1 = f add
        |    res2 = f sub
        |    res1 - res2
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  subject should "work with mixfix functions" in {
    val code =
      """
        |Number.if_then_else = ~t -> ~f -> ifZero this t f
        |
        |main =
        |    f = if _ then 10 else 5
        |    res1 = f 0
        |    res2 = f 1
        |    res1 - res2
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  subject should "work with case expressions" in {
    val code =
      """
        |main =
        |    f = case _ of
        |           Cons a b -> 10
        |           Nil -> 0
        |    res1 = f (Cons 1 2)
        |    res2 = f Nil
        |    res2 - res1
        |""".stripMargin

    eval(code) shouldEqual -10
  }

  subject should "mean id when used alone" in {
    val code =
      """
        |main =
        |    f = (x = _) -> x
        |    g = f.call
        |    h = _
        |    res1 = g 10
        |    res2 = h 10
        |    res1 - res2
        |""".stripMargin

    eval(code) shouldEqual 0
  }

  subject should "work with operators" in {
    val code =
      """
        |main =
        |     f = (_ + 10)
        |     f 10
        |""".stripMargin

    eval(code) shouldEqual 20
  }

  subject should "work properly with left operator sections" in {
    val code =
      """
        |main =
        |    f = (_ -)
        |    f 10 5
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  subject should "work properly with centre operator sections" in {
    val code =
      """
        |main =
        |    f = _ - _
        |    f 10 5
        |""".stripMargin

    eval(code) shouldEqual 5
  }

  subject should "work properly with right operator sections" in {
    val code =
      """
        |main =
        |    f = (+ _)
        |    f 10 5
        |""".stripMargin

    eval(code) shouldEqual 15
  }

  subject should "work properly with vector literals" in {
    val code =
      """
        |main =
        |    fun = [1, _, (1 + 2), _]
        |    vec = fun 2 4
        |    IO.println (Polyglot.get_array_element vec 0)
        |    IO.println (Polyglot.get_array_element vec 1)
        |    IO.println (Polyglot.get_array_element vec 2)
        |    IO.println (Polyglot.get_array_element vec 3)
        |
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("1", "2", "3", "4")
  }
}

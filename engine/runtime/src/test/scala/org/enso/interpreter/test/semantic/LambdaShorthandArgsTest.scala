package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class LambdaShorthandArgsTest extends InterpreterTest {

  override def subject = "Lambda shorthand arguments"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {
    "work for simple applications" in {
      val code =
        """
          |main =
          |    f = a -> b -> c -> a + b - c
          |    g = f _ 5 5
          |    g 10
          |""".stripMargin

      eval(code) shouldEqual 10
    }

    "work for named applications" in {
      val code =
        """
          |main =
          |    f = a -> b -> a - b
          |    g = f (b = _)
          |    g 10 5
          |""".stripMargin

      eval(code) shouldEqual -5
    }

    "work for functions in applications" in {
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

    "work with mixfix functions" in {
      val code =
        """from Builtins import all
          |
          |Number.if_then_else = ~t -> ~f -> if this == 0 then t else f
          |
          |main =
          |    f = if _ then 10 else 5
          |    res1 = f 0
          |    res2 = f 1
          |    res1 - res2
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "work with case expressions" in {
      val code =
        """from Builtins import all
          |
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

    "mean id when used alone" in {
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

    "work with operators" in {
      val code =
        """
          |main =
          |     f = (_ + 10)
          |     f 10
          |""".stripMargin

      eval(code) shouldEqual 20
    }

    "work properly with left operator sections" in {
      val code =
        """
          |main =
          |    f = (_ -)
          |    f 10 5
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "work properly with centre operator sections" in {
      val code =
        """
          |main =
          |    f = _ - _
          |    f 10 5
          |""".stripMargin

      eval(code) shouldEqual 5
    }

    "work properly with right operator sections" in {
      val code =
        """
          |main =
          |    f = (+ _)
          |    f 10 5
          |""".stripMargin

      eval(code) shouldEqual 15
    }

    "work properly with vector literals" in {
      val code =
        """from Builtins import all
          |
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

    "work properly when used with dot notation" in {
      val code =
        """
          |main =
          |    f = (+ 10)
          |    fun = _.f
          |    fun 10
          |""".stripMargin

      eval(code) shouldEqual 20
    }
  }
}

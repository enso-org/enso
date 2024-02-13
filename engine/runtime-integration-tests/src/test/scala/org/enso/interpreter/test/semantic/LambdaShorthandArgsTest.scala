package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class LambdaShorthandArgsTest extends InterpreterTest {

  override def subject = "Lambda shorthand arguments"

  override def specify(implicit
    interpreterContext: InterpreterContext
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
        """from Standard.Base.Data.Numbers import all
          |import Standard.Base.Any.Any
          |
          |Number.if_then_else self = ~t -> ~f -> if self == 0 then t else f
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
        """import Standard.Base.Data.List.List
          |
          |main =
          |    f = case _ of
          |           List.Cons a b -> 10
          |           List.Nil -> 0
          |    res1 = f (List.Cons 1 2)
          |    res2 = f List.Nil
          |    res2 - res1
          |""".stripMargin

      eval(code) shouldEqual -10
    }

    "mean id when used alone" in {
      val code =
        """
          |main =
          |    f = (x = _) -> x
          |    g = f
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

    "work properly when used with dot notation" in {
      val code =
        """
          |from Standard.Base.Data.Numbers import Number
          |
          |Number.f self = self + 10
          |
          |main =
          |    fun = _.f
          |    fun 10
          |""".stripMargin

      eval(code) shouldEqual 20
    }

    "work properly when used inside the function of an application" in {
      val code =
        """
          |main = (_ - 5) 0
          |""".stripMargin

      eval(code) shouldEqual -5
    }
  }
}

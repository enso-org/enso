package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class MixfixFunctionsTest extends InterpreterTest {
  override def subject = "Mixfix Functions"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "be able to be defined as a method" in {
      val code =
        """
          |from Standard.Base import all
          |type Foo
          |    Mk_Foo a
          |
          |Foo.if_then self = x -> case self of
          |    Foo.Mk_Foo a -> a + x
          |
          |main = (Foo.Mk_Foo 2).if_then 8
          |""".stripMargin

      eval(code) shouldEqual 10
    }

    "easily support multiple arguments" in {
      val code =
        """
          |type Foo
          |    Mk_Foo a b
          |
          |Foo.if_then_else self = a -> b -> case self of
          |    Foo.Mk_Foo x y -> x + y + a + b
          |
          |main = (Foo.Mk_Foo 1 2).if_then_else 3 4
          |""".stripMargin

      eval(code) shouldEqual 10
    }
  }
}

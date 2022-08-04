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
          |type Foo
          |    Mk_Foo a
          |
          |Foo.if_then self = x -> case self of
          |    Mk_Foo a -> a + x
          |
          |main = if Mk_Foo 2 then 8
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
          |    Mk_Foo x y -> x + y + a + b
          |
          |main = if (Mk_Foo 1 2) then 3 else 4
          |""".stripMargin

      eval(code) shouldEqual 10
    }
  }
}

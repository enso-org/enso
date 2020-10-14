package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class GroupingTest extends InterpreterTest {
  override def subject: String = "Parentheses"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "work with arbitrary lambdas" in {
      val code =
        """
          |main = (x -> x)
          |""".stripMargin

      eval(code).call(5) shouldEqual 5
    }

    "work with RHS of an assignment" in {
      val code =
        """
          |main =
          |    fn = (x -> x)
          |    fn 10
          |""".stripMargin

      eval(code) shouldEqual 10
    }

    "work with forced terms and lazy arguments" in {
      val code =
        """
          |main =
          |    ifTest = c -> (~ifT) -> ~ifF -> if c == 0 then ifT else ifF
          |    sum = c -> acc -> ifTest c acc (@Tail_Call sum c-1 acc+c)
          |    sum 10000 0
          |""".stripMargin

      eval(code) shouldEqual 50005000
    }

    "work with arbitrary arguments" in {
      val code =
        """
          |main = (x) -> x
          |""".stripMargin

      eval(code).call(5) shouldEqual 5
    }

    "work with pattern matches" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    fn = x -> case x of
          |        (Cons h t) -> h + fn t
          |        (_) -> 0
          |
          |    fn (Cons 7 Nil)
          |""".stripMargin

      eval(code) shouldEqual 7
    }

    "work with method calls" in {
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
}

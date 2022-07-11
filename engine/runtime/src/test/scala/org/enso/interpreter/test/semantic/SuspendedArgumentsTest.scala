package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class SuspendedArgumentsTest extends InterpreterTest {
  override def subject = "Suspended arguments"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "work in basic expressions" in {
      val code =
        """
          |main =
          |    lazyId : Suspended -> a
          |    lazyId = x -> x
          |
          |    lazyId (1 + 1)
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "not get executed upfront" in {
      val code =
        """import Standard.Base.IO
          |
          |main =
          |    foo = i -> ~x -> ~y -> if i == 0 then x else y
          |    foo 1 (IO.println 1) (IO.println 2)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("2")
    }

    "work well with tail recursion" in {
      val code =
        """
          |main =
          |    ifTest = c -> ~ifT -> ~ifF -> if c == 0 then ifT else ifF
          |    sum = c -> acc -> ifTest c acc (@Tail_Call sum c-1 acc+c)
          |    sum 10000 0
          |""".stripMargin
      eval(code) shouldEqual 50005000
    }

    "work in non-tail positions" in {
      val code =
        """
          |main =
          |    suspInc : Suspended -> Number
          |    suspInc = ~x -> 1 + x
          |    suspInc (suspInc 10)
          |""".stripMargin

      eval(code) shouldEqual 12
    }

    "work properly with method dispatch" in {
      val code =
        """import Standard.Base.IO
          |
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

    "work properly with oversaturated arguments" in {
      val code =
        """import Standard.Base.IO
          |
          |main =
          |    ifTest = c -> ~ifT -> ~ifF -> if c == 0 then ifT else ifF
          |    foo = c -> ifTest c
          |
          |    foo 0 (IO.println 1) (IO.println 2)
          |    foo 1 (IO.println 3) (IO.println 4)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("1", "4")
    }

    "work properly with defaulted arguments" in {
      val code =
        """from Standard.Base import all
          |
          |main = a -> (~b = Panic.throw 1) -> a
          |""".stripMargin
      eval(code).call(1) shouldEqual 1
    }

    "allow passing suspended functions" in {
      val code =
        """main =
          |    foo = ~x -> x 1
          |    foo (x -> x)
          |""".stripMargin

      eval(code) shouldEqual 1
    }
  }
}

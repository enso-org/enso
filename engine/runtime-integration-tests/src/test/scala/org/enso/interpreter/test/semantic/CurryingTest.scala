package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class CurryingTest extends InterpreterTest {
  override def subject: String = "Functions"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "allow partial application" in {
      val code =
        """
          |main =
          |    apply = v -> f -> f v
          |    adder = a -> b -> a + b
          |    plusOne = apply (f = adder 1)
          |    result = plusOne 10
          |    result
          |""".stripMargin

      eval(code) shouldEqual 11
    }

    "allow default arguments to be suspended" in {
      val code =
        """
          |main =
          |    fn = w -> x -> (y = 10) -> (z = 20) -> w + x + y + z
          |
          |    fn1 = fn ...
          |    fn2 = fn1 1 2 ...
          |    fn3 = fn2 3 ...
          |    fn3
          |""".stripMargin

      eval(code) shouldEqual 26
    }

    "be callable with arguments" in {
      val code =
        """
          |main =
          |    fn = w -> x -> (y = 10) -> (z = 20) -> w + x + y + z
          |
          |    fn 1 2 (z = 10)
          |""".stripMargin

      eval(code) shouldEqual 23
    }

    "allow defaults to be suspended in application chains" in {
      val code =
        """
          |main =
          |    fn = w -> x -> (y = 10) -> (z = 20) -> w + x + y + z
          |    id = x -> x
          |
          |    (fn 3 (id 6) ...) 3
          |""".stripMargin

      eval(code) shouldEqual 32
    }

    "allow default arguments to be suspended in method call syntax" in {
      val code =
        """import Standard.Base.Nothing
          |
          |Nothing.fn = w -> x -> (y = 10) -> (z = 20) -> w + x + y + z
          |
          |main =
          |    fn1 = Nothing.fn ...
          |    fn2 = fn1 1 2 ...
          |    fn3 = fn2 3 ...
          |    fn3
          |""".stripMargin

      eval(code) shouldEqual 26
    }

    "automatically force functions with all-defaulted arguments" in {
      val code =
        """main =
          |     foo (x=1) = x
          |     foo + 1
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "allow to pass suspended functions in arguments with `...`" in {
      val code =
        """main =
          |    foo f = f 2
          |    bar x=1 = x + 1
          |    foo (bar ...)
          |""".stripMargin
      eval(code) shouldEqual 3
    }

    "allow to pass suspended functions in arguments with `...` but still auto-execute them" in {
      val code =
        """main =
          |    foo f = f
          |    bar x=1 = x + 1
          |    foo (bar ...)
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "should handle a defaulted-suspended combo" in {
      val code =
        """main =
          |    foo ~f = f
          |    bar x=1 = x + 1
          |    foo (bar ...)
          |""".stripMargin
      eval(code) shouldEqual 2
    }

    "should make `...` an identity on Atom Constructors" in {
      val code =
        """
          |type My_Atom x=1
          |
          |My_Atom.my_static = "hello"
          |
          |main =
          |    (My_Atom ...).my_static
          |""".stripMargin
      eval(code) shouldEqual "hello"
    }

  }
}

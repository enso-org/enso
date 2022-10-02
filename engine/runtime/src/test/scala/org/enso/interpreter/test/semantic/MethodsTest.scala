package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class MethodsTest extends InterpreterTest {
  override def subject: String = "Methods"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "be defined in the global scope and dispatched to" in {
      val code =
        """
          |type Foo
          |Foo.bar = number -> number + 1
          |main = Foo.bar 10
          |""".stripMargin
      eval(code) shouldEqual 11
    }

    "execute `self` argument once" in {
      val code =
        """from Standard.Base.IO import all
          |import Standard.Base.Nothing
          |
          |Nothing.Nothing.foo = 0
          |
          |main = (IO.println "foo").foo
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("foo")
    }

    "be callable with dot operator" in {
      val code =
        """
          |type Foo
          |Foo.bar = number -> number + 1
          |main = Foo.bar 10
          |""".stripMargin
      eval(code) shouldEqual 11
    }

    "be chainable with dot operator" in {
      val code =
        """
          |type Foo
          |type Bar
          |type Baz
          |
          |Foo.bar = Bar
          |Bar.baz = x -> Baz
          |Baz.spam = y -> y + 25
          |
          |main = Foo.bar.baz 54 . spam 2
          |""".stripMargin
      eval(code) shouldEqual 27
    }

    "behave like parenthesised when called with non-spaced dot operator" in {
      val code =
        """
          |type Foo
          |type Bar
          |
          |Foo.bar = a -> b -> a + b
          |Bar.constant = 10
          |
          |main = Foo.bar Bar.constant Bar.constant
          |
          |""".stripMargin
      eval(code) shouldEqual 20
    }

    "be able to be defined without arguments" in {
      val code =
        """
          |type Foo
          |Foo.bar = 1
          |main = Foo.bar + 5
          |""".stripMargin
      eval(code) shouldEqual 6
    }

    "be definable as blocks without arguments" in {
      val code =
        """from Standard.Base.Data.Any import all
          |
          |Any.Any.method self =
          |    x = self * self
          |    y = x * 2
          |    y + 1
          |
          |main = 3.method
          |""".stripMargin
      eval(code) shouldEqual 19
    }

    "throw an exception when non-existent" in {
      val code =
        """
          |main = 7.foo
          |""".stripMargin
      the[InterpreterException] thrownBy eval(
        code
      ) should have message "Method `foo` of 7 (Integer) could not be found."
    }

    "be callable for any type when defined on Any" in {
      val code =
        """from Standard.Base.Data.Any import all
          |import Standard.Base.IO
          |import Standard.Base.Nothing
          |
          |type Foo
          |type Bar
          |type Baz
          |
          |Any.Any.method self = case self of
          |    Foo -> 1
          |    Bar -> 2
          |    Baz -> 3
          |    _ -> 0
          |
          |main =
          |    IO.println Foo.method
          |    IO.println Bar.method
          |    IO.println Baz.method
          |    IO.println Nothing.method
          |    IO.println 123.method
          |    IO.println (x -> x).method
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("1", "2", "3", "0", "0", "0")
    }

    "be callable for any type when defined on Any (resolved as a type name)" in {
      import annotation.unused
      @unused val code =
        """from Standard.Base.Data.Any import all
          |
          |Any.method self = 1
          |
          |main =
          |    2.method
          |""".stripMargin
//      eval(code) shouldEqual 1
      pending
    }

    "be callable on types when static" in {
      val code =
        """
          |type Foo
          |    Mk_Foo a
          |
          |    new a = Foo.Mk_Foo a
          |
          |main = Foo.new 123
          |""".stripMargin
      eval(code).toString shouldEqual "(Mk_Foo 123)"
    }

    "not be callable on types when non-static" in {
      val code =
        """
          |type Foo
          |    Mk_Foo a
          |
          |    inc self = Mk_Foo self.a
          |
          |main = Foo.inc
          |""".stripMargin
      the[InterpreterException] thrownBy eval(
        code
      ) should have message "Method `inc` of Foo could not be found."
    }

    "not be callable on instances when static" in {
      val code =
        """
          |type Foo
          |    Mk_Foo a
          |
          |    new a = Foo.Mk_Foo a
          |
          |main = Foo.Mk_Foo 123 . new 123
          |""".stripMargin
      the[InterpreterException] thrownBy eval(
        code
      ) should have message "Method `new` of Mk_Foo could not be found."
    }
  }
}

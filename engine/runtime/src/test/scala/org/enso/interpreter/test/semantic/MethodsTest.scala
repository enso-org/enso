package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterTest,
  InterpreterContext,
  InterpreterException
}

class MethodsTest extends InterpreterTest {
  override def subject: String = "Methods"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {
    "be defined in the global scope and dispatched to" in {
      val code =
        """
          |type Foo
          |Foo.bar = number -> number + 1
          |main = bar Foo 10
          |""".stripMargin
      eval(code) shouldEqual 11
    }
    "execute `this` argument once" in {
      val code =
        """from Builtins import all
          |
          |Unit.foo = 0
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
          |main = bar Foo + 5
          |""".stripMargin
      eval(code) shouldEqual 6
    }

    "be definable as blocks without arguments" in {
      val code =
        """from Builtins import all
          |
          |Any.method =
          |    x = this * this
          |    y = x * 2
          |    y + 1
          |
          |main = 3.method
          |""".stripMargin
      eval(code) shouldEqual 19
    }

    "be dispatched to the proper constructor" in {
      val code =
        """from Builtins import all
          |
          |Nil.sum = acc -> acc
          |Cons.sum = acc -> case this of
          |  Cons h t -> sum t (h + acc)
          |
          |main = sum (Cons 1 (Cons 2 Nil)) 0
          |""".stripMargin

      eval(code) shouldEqual 3
    }

    "allow passing the call target by-name" in {
      val code =
        """from Builtins import all
          |
          |Unit.testMethod = x -> y -> z -> x + y + z
          |main = testMethod x=1 y=2 this=Unit z=3
          |""".stripMargin
      eval(code) shouldEqual 6
    }

    "throw an exception when non-existent" in {
      val code =
        """
          |main = foo 7
          |""".stripMargin
      the[InterpreterException] thrownBy eval(code) should have message "Object Integer does not define method foo."
    }

    "be callable for any type when defined on Any" in {
      val code =
        """from Builtins import all
          |
          |type Foo
          |type Bar
          |type Baz
          |
          |Any.method = case this of
          |  Foo -> 1
          |  Bar -> 2
          |  Baz -> 3
          |  _ -> 0
          |
          |main =
          |    IO.println Foo.method
          |    IO.println Bar.method
          |    IO.println Baz.method
          |    IO.println Unit.method
          |    IO.println 123.method
          |    IO.println (x -> x).method
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("1", "2", "3", "0", "0", "0")
    }

    "work as expected when defined across different constructors" in {
      val code =
        """from Builtins import all
          |
          |Nil.sum = 0
          |Cons.sum = case this of
          |  Cons h t -> h + sum t
          |
          |main =
          |    myList = Cons 1 (Cons 2 (Cons 3 Nil))
          |    myList.sum
          |
          |""".stripMargin

      eval(code) shouldEqual 6
    }
  }
}

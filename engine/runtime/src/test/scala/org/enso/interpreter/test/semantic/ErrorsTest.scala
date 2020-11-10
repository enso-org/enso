package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class ErrorsTest extends InterpreterTest {
  override def subject: String = "Errors and Panics"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "be thrown and stop evaluation" in {
      val code =
        """from Builtins import all
          |
          |type Foo
          |type Bar
          |type Baz
          |
          |main =
          |    IO.println Foo
          |    Panic.throw Bar
          |    IO.println Baz
          |""".stripMargin

      val exception = the[InterpreterException] thrownBy eval(code)
      exception.isGuestException shouldEqual true
      exception.getGuestObject.toString shouldEqual "Bar"
      consumeOut shouldEqual List("Foo")
    }

    "be recoverable and transformed into errors" in {
      val code =
        """from Builtins import all
          |
          |type MyError
          |
          |main =
          |    thrower = x -> Panic.throw x
          |    caught = Panic.recover (thrower MyError)
          |    IO.println caught
          |""".stripMargin

      noException shouldBe thrownBy(eval(code))
      consumeOut shouldEqual List("(Error: MyError)")
    }

    "propagate through pattern matches" in {
      val code =
        """from Builtins import all
          |
          |type MyError
          |
          |main =
          |    brokenVal = Error.throw MyError
          |    matched = case brokenVal of
          |        Nothing -> 1
          |        _ -> 0
          |
          |    IO.println matched
          |""".stripMargin
      noException shouldBe thrownBy(eval(code))
      consumeOut shouldEqual List("(Error: MyError)")
    }

    "propagate through specialized pattern matches" in {
      val code =
        """from Builtins import all
          |
          |type MyError
          |
          |main =
          |    brokenVal = Error.throw MyError
          |    f = case _ of
          |        Nothing -> 1
          |        _ -> 0
          |
          |    IO.println (f Nothing)
          |    IO.println (f brokenVal)
          |""".stripMargin
      noException shouldBe thrownBy(eval(code))
      consumeOut shouldEqual List("1", "(Error: MyError)")
    }

    "be catchable by a user-provided special handling function" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    intError = Error.throw 1
          |    intError.catch (x -> x + 3)
          |""".stripMargin
      eval(code) shouldEqual 4
    }

    "accept a constructor handler in catch function" in {
      val code =
        """from Builtins import all
          |
          |type MyCons err
          |
          |main =
          |    unitErr = Error.throw Nothing
          |    IO.println (unitErr.catch MyCons)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(MyCons Nothing)")
    }

    "accept a method handle in catch function" in {
      val code =
        """from Builtins import all
          |
          |type MyRecovered x
          |type MyError x
          |
          |MyError.recover = case this of
          |    MyError x -> MyRecovered x
          |
          |main =
          |    myErr = Error.throw (MyError 20)
          |    IO.println(myErr.catch recover)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(MyRecovered 20)")
    }

    "make the catch method an identity for non-error values" in {
      val code = "main = 10.catch (x -> x + 1)"
      eval(code) shouldEqual 10
    }

    "catch polyglot errors" in {
      val code =
        """from Builtins import all
          |polyglot java import java.lang.Long
          |
          |main =
          |    caught = Panic.recover (Long.parseLong (Array.new_1 "oops"))
          |    IO.println caught
          |    cause = caught.catch <| case _ of
          |        Polyglot_Error err -> err
          |        _ -> "fail"
          |    IO.println cause
          |    message = cause.getMessage (Array.new 0)
          |    IO.println message
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List(
        """(Error: (Polyglot_Error java.lang.NumberFormatException: For input string: "oops"))""",
        """java.lang.NumberFormatException: For input string: "oops"""",
        """For input string: "oops""""
      )
    }
  }
}

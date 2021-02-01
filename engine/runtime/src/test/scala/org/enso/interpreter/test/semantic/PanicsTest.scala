package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class PanicsTest extends InterpreterTest {
  override def subject: String = "Panics"

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

    "catch polyglot errors" in {
      val code =
        """from Builtins import all
          |polyglot java import java.lang.Long
          |
          |main =
          |    caught = Panic.recover (Long.parseLong "oops")
          |    IO.println caught
          |    cause = caught.catch <| case _ of
          |        Polyglot_Error err -> err
          |        _ -> "fail"
          |    IO.println cause
          |    message = cause.getMessage
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

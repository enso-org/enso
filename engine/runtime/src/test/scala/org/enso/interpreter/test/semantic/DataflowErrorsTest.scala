package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class DataflowErrorsTest extends InterpreterTest {
  override def subject: String = "Dataflow Errors"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

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
          |    IO.println(myErr.catch .recover)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("(MyRecovered 20)")
    }

    "make the catch method an identity for non-error values" in {
      val code = "main = 10.catch (x -> x + 1)"
      eval(code) shouldEqual 10
    }
  }

  // TODO [AA] Builtins need to handle a variety of cases around laziness in arguments.
}

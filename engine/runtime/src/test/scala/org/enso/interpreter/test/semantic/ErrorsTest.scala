package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class ErrorsTest extends InterpreterTest {
  "Panics" should "be thrown and stop evaluation" in {
    val code =
      """
        |type Foo
        |type Bar
        |type Baz
        |
        |IO.println Foo
        |Panic.throw Bar
        |IO.println Baz
        |""".stripMargin

    val exception = the[InterpreterException] thrownBy eval(code)
    exception.isGuestException shouldEqual true
    exception.getGuestObject.toString shouldEqual "Bar"
    consumeOut shouldEqual List("Foo")
  }

  "Panics" should "be recoverable and transformed into errors" in {
    val code =
      """
        |type MyError
        |
        |thrower = x -> Panic.throw x
        |caught = Panic.recover (thrower MyError)
        |IO.println caught
        |""".stripMargin

    noException shouldBe thrownBy(eval(code))
    consumeOut shouldEqual List("Error:MyError")
  }

  "Errors" should "propagate through pattern matches" in {
    val code =
      """
        |type MyError
        |
        |brokenVal = Error.throw MyError
        |matched = case brokenVal of
        |  Unit -> 1
        |  _ -> 0
        |
        |IO.println matched
        |""".stripMargin
    eval(code)
    noException shouldBe thrownBy(eval(code))
    consumeOut shouldEqual List("Error:MyError")
  }

  "Errors" should "be catchable by a user-provided special handling function" in {
    val code =
      """
        |intError = Error.throw 1
        |intError.catch (x -> x + 3)
        |""".stripMargin
    eval(code) shouldEqual 4
  }

  "Catch function" should "accept a constructor handler" in {
    val code =
      """
        |type MyCons err
        |
        |unitErr = Error.throw Unit
        |IO.println (unitErr.catch MyCons)
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("MyCons Unit")
  }

  "Catch function" should "accept a method handler" in {
    val code =
      """
        |type MyRecovered x
        |type MyError x
        |
        |MyError.recover = case this of
        |  MyError x -> MyRecovered x
        |
        |myErr = Error.throw (MyError 20)
        |
        |IO.println(myErr.catch recover)
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("MyRecovered 20")
  }

  "Catch function" should "act as identity for non-error values" in {
    val code = "10.catch (x -> x + 1)"
    eval(code) shouldEqual 10
  }
}

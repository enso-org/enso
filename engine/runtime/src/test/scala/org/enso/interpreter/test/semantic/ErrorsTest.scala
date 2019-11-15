package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class ErrorsTest extends InterpreterTest {
  "Panics" should "be thrown and stop evaluation" in {
    val code =
      """
        |type Foo;
        |type Bar;
        |type Baz;
        |@{
        |  @println [@IO, @Foo];
        |  @throw [@Panic, @Bar];
        |  @println [@IO, @Baz]
        |}
        |""".stripMargin

    val exception = the[InterpreterException] thrownBy eval(code)
    exception.isGuestException shouldEqual true
    exception.getGuestObject.toString shouldEqual "Bar<>"
    consumeOut shouldEqual List("Foo<>")
  }

  "Panics" should "be recoverable and transformed into errors" in {
    val code =
      """
        |type MyError;
        |
        |@{
        |  thrower = { |x| @throw [@Panic, x] };
        |  caught = @recover [@Panic, @thrower [@MyError]];
        |  @println [@IO, caught]
        |}
        |""".stripMargin

    noException shouldBe thrownBy(eval(code))
    consumeOut shouldEqual List("Error:MyError<>")
  }

  "Errors" should "propagate through pattern matches" in {
    val code =
      """
        |type MyError;
        |
        |@{
        |  brokenVal = @throw [@Error, @MyError];
        |  matched = match brokenVal <
        |    Unit ~ { 1 };
        |    {0};
        |  >;
        |  @println [@IO, matched]
        |}
        |""".stripMargin
    eval(code)
    noException shouldBe thrownBy(eval(code))
    consumeOut shouldEqual List("Error:MyError<>")
  }

  "Errors" should "be catchable by a user-provided special handling function" in {
    val code =
      """
        |@{
        |  intError = @throw [@Error, 1];
        |  @catch [intError, { |x| x + 3 }]
        |}
        |""".stripMargin
    eval(code) shouldEqual 4
  }

  "Catch function" should "accept a constructor handler" in {
    val code =
      """
        |type MyCons err;
        |
        |@{
        |  unitErr = @throw [@Error, @Unit];
        |  @println [@IO, @catch [unitErr, MyCons]]
        |}
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("MyCons<Unit<>>")
  }

  "Catch function" should "accept a method handler" in {
    val code =
      """
        |type MyRecovered x;
        |type MyError x;
        |
        |MyError.recover = match this <
        |  MyError ~ { |x| @MyRecovered [x] };
        |>
        |
        |@{
        |   myErr = @throw [@Error, @MyError [20]];
        |   @println [@IO, @catch [myErr, recover]]
        |}
        |
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("MyRecovered<20>")
  }

  "Catch function" should "act as identity for non-error values" in {
    val code = "@catch [10, {|x| x + 1}]"
    eval(code) shouldEqual 10
  }
}

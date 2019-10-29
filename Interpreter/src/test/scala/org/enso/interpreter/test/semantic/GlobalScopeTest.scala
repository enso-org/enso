package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, LanguageTest}

class GlobalScopeTest extends LanguageTest {

  "Variables" should "be able to be read from the global scope" in {
    val code =
      """
        |Unit.a = 10
        |
        |@a [@Unit]
    """.stripMargin

    eval(code) shouldEqual 10
  }

  "Functions" should "use values from the global scope in their bodies" in {
    val code =
      """
        |Unit.a = 10
        |Unit.addTen = { |b| (@a [@Unit]) + b }
        |
        |@addTen [@Unit, 5]
    """.stripMargin

    eval(code) shouldEqual 15
  }

  "Functions" should "be able to call other functions in scope" in {
    val code =
      """
        |Unit.adder = { |a, b| a + b }
        |
        |@{ |multiply|
        |  res = @adder [@Unit, 1, 2];
        |  doubled = res * multiply;
        |  doubled
        |} [2]
    """.stripMargin

    eval(code) shouldEqual 6
  }

  "Functions" should "be able to be passed as values when in scope" in {
    val code =
      """
        |Unit.adder = { |a, b| a + b }
        |
        |Unit.binaryFn = { |a, b, function|
        |  result = @function [a, b];
        |  result
        |}
        |
        |@binaryFn [@Unit, 1, 2, { |a, b| @adder [@Unit, a, b] }]
    """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be able to mutually recurse in the global scope" in {
    val code =
      """
        |Unit.decrementCall = { |number|
        |  res = number - 1;
        |  @fn1 [@Unit, res]
        |}
        |
        |Unit.fn1 = { |number|
        |  ifZero: [number % 3, number, @decrementCall [@Unit, number]]
        |}
        |
        |@fn1 [@Unit, 5]
      """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be suspended within blocks" in {
    val code =
      """
        |Unit.a = 10/0
        |
        |Unit.b = { @a [@Unit] }
        |b
    """.stripMargin

    noException should be thrownBy eval(code)
  }

  "Exceptions" should "be thrown when called" in {
    val code =
      """
        |Unit.a = 10/0
        |
        |Unit.b = { @a [@Unit] }
        |@b [@Unit]
      """.stripMargin

    an[InterpreterException] should be thrownBy eval(code)
  }

}

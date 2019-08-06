package org.enso.interpreter

import org.graalvm.polyglot.PolyglotException

class GlobalScopeTest extends LanguageTest {
  // TODO [AA] Should work with bare vars.
  "Variables" should "be able to be read from the global scope" in {
    val code =
      """
        |a = 10
        |
        |a
    """.stripMargin

    eval(code) shouldEqual 10
  }

  "Functions" should "use values from the global scope in their bodies" in {
    val code =
      """
        |a = 10
        |addTen = { |b| a + b }
        |
        |@addTen [5]
    """.stripMargin

    eval(code) shouldEqual 15
  }

  "Functions" should "be able to call other functions in scope" in {
    val code =
      """
        |adder = { |a, b| a + b }
        |
        |@{ |multiply|
        |  res = @adder [1, 2];
        |  doubled = res * multiply;
        |  doubled
        |} [2]
    """.stripMargin

    eval(code) shouldEqual 6
  }

  "Functions" should "be able to be passed as values when in scope" in {
    val code =
      """
        |adder = { |a, b| a + b }
        |
        |binaryFn = { |a, b, function|
        |  result = @function [a, b];
        |  result
        |}
        |
        |@binaryFn [1, 2, adder]
    """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be able to mutually recurse in the global scope" in {
    val code =
      """
        |decrementCall = { |number|
        |  res = number - 1;
        |  @fn1 [res]
        |}
        |
        |fn1 = { |number|
        |  ifZero: [number % 3, number, @decrementCall [number]]
        |}
        |
        |@fn1 [5]
      """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be suspended within blocks" in {
    val code =
      """
        |a = 10/0
        |
        |b = { @a }
        |b
    """.stripMargin

    noException should be thrownBy eval(code)
  }

  "Exceptions" should "be thrown when called" in {
    val code =
      """
        |a = 10/0
        |
        |b = { @a }
        |@b
      """.stripMargin

    a[PolyglotException] should be thrownBy eval(code)
  }

}

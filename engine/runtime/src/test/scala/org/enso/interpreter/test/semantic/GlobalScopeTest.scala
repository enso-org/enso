package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}

class GlobalScopeTest extends InterpreterTest {

  "Variables" should "be able to be read from the global scope" in {
    val code =
      """
        |Unit.a = 10
        |
        |main = a Unit
    """.stripMargin

    eval(code) shouldEqual 10
  }

  "Functions" should "use values from the global scope in their bodies" in {
    val code =
      """
        |Unit.a = 10
        |Unit.addTen = b -> a Unit + b
        |
        |main = addTen Unit 5
    """.stripMargin

    eval(code) shouldEqual 15
  }

  "Functions" should "be able to call other functions in scope" in {
    val code =
      """
        |Unit.adder = a -> b -> a + b
        |
        |main =
        |    fn = multiply ->
        |        res = adder Unit 1 2
        |        doubled = res * multiply
        |        doubled
        |    fn 2
    """.stripMargin

    eval(code) shouldEqual 6
  }

  "Functions" should "be able to be passed as values when in scope" in {
    val code =
      """
        |Unit.adder = a -> b -> a + b
        |
        |Unit.binaryFn = a -> b -> function ->
        |  result = function a b
        |  result
        |
        |main = Unit.binaryFn 1 2 (a -> b -> Unit.adder a b)
    """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be able to mutually recurse in the global scope" in {
    val code =
      """
        |Unit.decrementCall = number ->
        |  res = number - 1
        |  Unit.fn1 res
        |
        |Unit.fn1 = number ->
        |  ifZero (number % 3) number (Unit.decrementCall number)
        |
        |main = Unit.fn1 5
      """.stripMargin

    eval(code) shouldEqual 3
  }

  "Functions" should "be suspended within blocks" in {
    val code =
      """
        |Unit.a = 10/0
        |
        |Unit.b = Unit.a
        |main = b
    """.stripMargin

    noException should be thrownBy eval(code)
  }

  "Exceptions" should "be thrown when called" in {
    val code =
      """
        |Unit.a = 10/0
        |
        |Unit.b = Unit.a
        |main = Unit.b
      """.stripMargin

    an[InterpreterException] should be thrownBy eval(code)
  }

  "Suspended blocks" should "work properly in the global scope" in {
    val code =
      """
        |main =
        |    myFun =
        |        IO.println 10
        |        0
        |
        |    IO.println 5
        |    myFun
        |""".stripMargin

    eval(code) shouldEqual 0
    consumeOut shouldEqual List("5", "10")
  }

  "Suspended blocks" should "be properly suspended" in {
    val code =
      """
        |main =
        |    block =
        |        State.put 0
        |
        |    State.put 5
        |    IO.println State.get
        |    block
        |    IO.println State.get
        |""".stripMargin

    eval(code)
    consumeOut shouldEqual List("5", "0")
  }
}

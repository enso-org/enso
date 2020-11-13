package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class GlobalScopeTest extends InterpreterTest {

  override def subject: String = "Functions"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "use values from the global scope in their bodies" in {
      val code =
        """
          |Unit.a = 10
          |Unit.addTen = b -> a Unit + b
          |
          |main = addTen Unit 5
        """.stripMargin

      eval(code) shouldEqual 15
    }

    "be able to call other functions in scope" in {
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

    "be able to be passed as values when in scope" in {
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

    "be able to mutually recurse in the global scope" in {
      val code =
        """
          |Unit.decrementCall = number ->
          |  res = number - 1
          |  Unit.fn1 res
          |
          |Unit.fn1 = number ->
          |  if (number % 3) == 0 then number else Unit.decrementCall number
          |
          |main = Unit.fn1 5
        """.stripMargin

      eval(code) shouldEqual 3
    }

    "be suspended within blocks" in {
      val code =
        """
          |Unit.a = 10/0
          |
          |Unit.b = Unit.a
          |main = b
        """.stripMargin

      noException should be thrownBy eval(code)
    }
  }
}

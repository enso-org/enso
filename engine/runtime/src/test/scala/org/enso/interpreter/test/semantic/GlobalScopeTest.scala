package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class GlobalScopeTest extends InterpreterTest {

  override def subject: String = "Functions"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "use values from the global scope in their bodies" in {
      val code =
        """from Builtins import all
          |
          |Nothing.a = 10
          |Nothing.addTen = b -> a Nothing + b
          |
          |main = addTen Nothing 5
        """.stripMargin

      eval(code) shouldEqual 15
    }

    "be able to call other functions in scope" in {
      val code =
        """from Builtins import all
          |
          |Nothing.adder = a -> b -> a + b
          |
          |main =
          |    fn = multiply ->
          |        res = adder Nothing 1 2
          |        doubled = res * multiply
          |        doubled
          |    fn 2
        """.stripMargin

      eval(code) shouldEqual 6
    }

    "be able to be passed as values when in scope" in {
      val code =
        """from Builtins import all
          |
          |Nothing.adder = a -> b -> a + b
          |
          |Nothing.binaryFn = a -> b -> function ->
          |  result = function a b
          |  result
          |
          |main = Nothing.binaryFn 1 2 (a -> b -> Nothing.adder a b)
        """.stripMargin

      eval(code) shouldEqual 3
    }

    "be able to mutually recurse in the global scope" in {
      val code =
        """from Builtins import all
          |
          |Nothing.decrementCall = number ->
          |  res = number - 1
          |  Nothing.fn1 res
          |
          |Nothing.fn1 = number ->
          |  if (number % 3) == 0 then number else Nothing.decrementCall number
          |
          |main = Nothing.fn1 5
        """.stripMargin

      eval(code) shouldEqual 3
    }

    "be suspended within blocks" in {
      val code =
        """from Builtins import all
          |
          |Nothing.a = 10/0
          |
          |Nothing.b = Nothing.a
          |main = b
        """.stripMargin

      noException should be thrownBy eval(code)
    }
  }
}

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
          |None.a = 10
          |None.addTen = b -> a None + b
          |
          |main = addTen None 5
        """.stripMargin

      eval(code) shouldEqual 15
    }

    "be able to call other functions in scope" in {
      val code =
        """from Builtins import all
          |
          |None.adder = a -> b -> a + b
          |
          |main =
          |    fn = multiply ->
          |        res = adder None 1 2
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
          |None.adder = a -> b -> a + b
          |
          |None.binaryFn = a -> b -> function ->
          |  result = function a b
          |  result
          |
          |main = None.binaryFn 1 2 (a -> b -> None.adder a b)
        """.stripMargin

      eval(code) shouldEqual 3
    }

    "be able to mutually recurse in the global scope" in {
      val code =
        """from Builtins import all
          |
          |None.decrementCall = number ->
          |  res = number - 1
          |  None.fn1 res
          |
          |None.fn1 = number ->
          |  if (number % 3) == 0 then number else None.decrementCall number
          |
          |main = None.fn1 5
        """.stripMargin

      eval(code) shouldEqual 3
    }

    "be suspended within blocks" in {
      val code =
        """from Builtins import all
          |
          |None.a = 10/0
          |
          |None.b = None.a
          |main = b
        """.stripMargin

      noException should be thrownBy eval(code)
    }
  }
}

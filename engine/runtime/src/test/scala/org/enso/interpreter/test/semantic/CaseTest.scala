package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}

class CaseTest extends InterpreterTest {
  override def subject = "Case expressions"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "result in an error if the matched constructor isn't visible" in {
      val code =
        """
          |from Standard.Base.Data.List import List
          |
          |main =
          |    x = List.Cons 0 List.Nil
          |    case x of
          |        Cons2 a b -> a + b
          |        Nil2 -> 0
          |""".stripMargin

      val msg =
        "Compile error: Cons2 is not visible in this scope, Nil2 is not visible in this scope"
      the[InterpreterException] thrownBy eval(code) should have message msg
    }

    "result in an error if the wrong number of fields are provided" in {
      val code =
        """
          |from Standard.Base.Data.List import List
          |
          |main =
          |    x = List.Cons 0 List.Nil
          |    case x of
          |        List.Cons a -> a
          |""".stripMargin

      val msg =
        "Compile error: Cannot match on List.Cons using 1 field (expecting 2)"
      the[InterpreterException] thrownBy eval(code) should have message msg
    }

    "result in an error when trying to pattern match on a module in a type position" in {
      val code =
        """
          |import Standard.Base.Data.Vector
          |
          |main =
          |    case [1,2,3] of
          |        _ : Vector -> 1
          |        _ -> 2
          |""".stripMargin

      val msg =
        "Compile error: Vector is not visible in this scope"
      the[InterpreterException] thrownBy eval(code) should have message msg
    }
  }
}

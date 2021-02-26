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
          |from Builtins import all
          |
          |main =
          |    x = Cons 0 Nil
          |    case x of
          |        Cons2 a b -> a + b
          |        Nil2 -> 0
          |""".stripMargin

      val msg = "Panic exception: Compile_Error"
      the[InterpreterException] thrownBy eval(code) should have message msg
    }

    "result in an error if the wrong number of fields are provided" in {
      val code =
        """
          |from Builtins import all
          |
          |main =
          |    x = Cons 0 Nil
          |    case x of
          |        Cons a -> a
          |""".stripMargin

      val msg = "Panic exception: Compile_Error"
      the[InterpreterException] thrownBy eval(code) should have message msg
    }
  }
}

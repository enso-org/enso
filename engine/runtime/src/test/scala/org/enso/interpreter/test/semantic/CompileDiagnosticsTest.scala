package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class CompileDiagnosticsTest extends InterpreterTest {
  override def subject: String = "Compile Error Reporting"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {
    "surface ast-processing errors in the language" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    x = Panic.recover ()
          |    x.catch err->
          |        case err of
          |            Syntax_Error msg -> "Oopsie, it's a syntax error: " + msg
          |""".stripMargin
      eval(code) shouldEqual "Oopsie, it's a syntax error: Parentheses can't be empty."
    }

    "surface parsing errors in the language" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    x = Panic.recover @
          |    x.catch to_text
          |""".stripMargin
      eval(code) shouldEqual "Syntax_Error Unrecognized token."
    }

    "surface redefinition errors in the language" in {
      val code =
        """from Builtins import all
          |
          |foo =
          |    x = 1
          |    x = 2
          |
          |main = Panic.recover here.foo . catch to_text
          |""".stripMargin
      eval(code) shouldEqual "Compile_Error Variable x is being redefined."
    }
  }
}

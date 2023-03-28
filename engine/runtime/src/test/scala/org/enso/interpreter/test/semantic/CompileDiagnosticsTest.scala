package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class CompileDiagnosticsTest extends InterpreterTest {
  override def subject: String = "Compile Error Reporting"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "surface ast-processing errors in the language" in {
      val code =
        """from Standard.Base.Errors.Common import Syntax_Error
          |import Standard.Base.Panic.Panic
          |import Standard.Base.Any.Any
          |
          |main =
          |    x = Panic.catch Any () .convert_to_dataflow_error
          |    x.catch_primitive err->
          |        case err of
          |            Syntax_Error.Error msg -> "Oopsie, it's a syntax error: " + msg
          |""".stripMargin
      eval(
        code
      ) shouldEqual "Oopsie, it's a syntax error: Parentheses can't be empty."
    }

    "surface parsing errors in the language" in {
      val code =
        """from Standard.Base.Errors.Common import all
          |import Standard.Base.Panic.Panic
          |
          |main =
          |    x = Panic.catch_primitive ` caught_panic-> caught_panic.payload
          |    x.to_text
          |""".stripMargin
      eval(code) shouldEqual "(Syntax_Error.Error 'Unexpected expression.')"
    }

    "surface redefinition errors in the language" in {
      val code =
        """from Standard.Base.Errors.Common import all
          |import Standard.Base.Panic.Panic
          |import Standard.Base.Any.Any
          |
          |foo =
          |    x = 1
          |    x = 2
          |
          |main = Panic.catch Any foo caught_panic->caught_panic.payload.to_text
          |""".stripMargin
      eval(
        code
      ) shouldEqual "(Compile_Error.Error 'Variable x is being redefined.')"
    }

    "surface non-existent variable errors in the language" in {
      val code =
        """from Standard.Base.Errors.Common import all
          |import Standard.Base.Panic.Panic
          |import Standard.Base.Any.Any
          |
          |foo =
          |    my_var = 10
          |    my_vra
          |
          |main = Panic.catch Any foo caught_panic-> caught_panic.payload.to_text
          |""".stripMargin
      eval(
        code
      ) shouldEqual "(Compile_Error.Error 'The name `my_vra` could not be found.')"
    }
  }
}

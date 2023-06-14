package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}
import org.enso.polyglot.RuntimeOptions
import org.graalvm.polyglot.Context

class StrictCompileDiagnosticsTest extends InterpreterTest {
  override def subject: String = "Compile Errors in Batch Mode"

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_.option(RuntimeOptions.STRICT_ERRORS, "true"))

  private def isDiagnosticLine(line: String): Boolean = {
    line.contains(" | ")
  }

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "be reported and abort execution" in {
      val code =
        """main =
          |    x = ()
          |    x = 5
          |    y = `
          |""".stripMargin.linesIterator.mkString("\n")
      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val errors = consumeOut
      errors
        .filterNot(isDiagnosticLine)
        .toSet shouldEqual Set(
        "Test:2:9: error: Parentheses can't be empty.",
        "Test:3:5: error: Variable x is being redefined.",
        "Test:4:9: error: Unexpected expression.",
        "Test:4:5: warning: Unused variable y.",
        "Test:2:5: warning: Unused variable x."
      )
    }
  }
}

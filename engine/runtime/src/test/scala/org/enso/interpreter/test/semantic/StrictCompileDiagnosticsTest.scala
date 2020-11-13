package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterTest,
  InterpreterContext,
  InterpreterException
}
import org.enso.polyglot.RuntimeOptions
import org.graalvm.polyglot.Context

class StrictCompileDiagnosticsTest extends InterpreterTest {
  override def subject: String = "Compile Errors in Batch Mode"

  override def contextModifiers: Context#Builder => Context#Builder =
    _.option(RuntimeOptions.STRICT_ERRORS, "true")

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "be reported and abort execution" in {
      val code =
        """main =
          |    x = ()
          |    x = 5
          |    y = @
          |""".stripMargin.linesIterator.mkString("\n")
      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val errors = consumeOut
      errors
        .filterNot(_.contains("Compiler encountered"))
        .toSet shouldEqual Set(
        "Test[2:9-2:10]: Parentheses can't be empty.",
        "Test[3:5-3:9]: Variable x is being redefined.",
        "Test[4:9-4:9]: Unrecognized token.",
        "Test[4:5-4:5]: Unused variable y.",
        "Test[2:5-2:5]: Unused variable x."
      )
    }
  }
}

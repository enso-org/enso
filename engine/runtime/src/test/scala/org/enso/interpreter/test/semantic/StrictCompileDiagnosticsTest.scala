package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}
import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.graalvm.polyglot.Context

class StrictCompileDiagnosticsTest extends InterpreterTest {
  override val ctx: Context = Context
    .newBuilder(LanguageInfo.ID)
    .allowExperimentalOptions(true)
    .option(RuntimeOptions.STRICT_ERRORS, "true")
    .out(output)
    .build()

  "Compile errors in batch mode" should "be reported and abort execution" in {
    val code =
      """main =
        |    x = ()
        |    x = 5
        |    y = @
        |""".stripMargin.linesIterator.mkString("\n")
    the[InterpreterException] thrownBy eval(code) should have message
    "Compilation aborted due to errors."

    val errors = consumeOut
    errors.filterNot(_.contains("Compiler encountered")).toSet shouldEqual Set(
      "Test[2:9-2:10]: Parentheses can't be empty.",
      "Test[3:5-3:9]: Variable x is being redefined.",
      "Test[4:9-4:9]: Unrecognized token.",
      "Test[4:5-4:5]: Unused variable y.",
      "Test[2:5-2:5]: Unused variable x."
    )
  }
}

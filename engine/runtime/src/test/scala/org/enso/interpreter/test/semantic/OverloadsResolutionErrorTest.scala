package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, InterpreterTest}
import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.graalvm.polyglot.Context

class OverloadsResolutionErrorTest extends InterpreterTest {

  // === Test Setup ===========================================================

  override val ctx: Context = Context
    .newBuilder(LanguageInfo.ID)
    .allowExperimentalOptions(true)
    .option(RuntimeOptions.STRICT_ERRORS, "true")
    .out(output)
    .build()

  // === The Tests ============================================================

  "Method overloads" should "result in an error at runtime" in {
    val code =
      """
        |Unit.foo = 10
        |Unit.foo = 20
        |""".stripMargin.linesIterator.mkString("\n")

    the[InterpreterException] thrownBy eval(code) should have message
    "Compilation aborted due to errors."

    val diagnostics = consumeOut
    diagnostics
      .filterNot(_.contains("Compiler encountered"))
      .toSet shouldEqual Set(
      "Test[3:1-3:13]: Method overloads are not supported: Unit.foo is defined multiple times in this module."
    )
  }

  "Atom overloads" should "result in an error at runtime" in {
    val code =
      """
        |type MyAtom
        |type MyAtom
        |""".stripMargin.linesIterator.mkString("\n")

    the[InterpreterException] thrownBy eval(code) should have message
    "Compilation aborted due to errors."

    val diagnostics = consumeOut
    diagnostics
      .filterNot(_.contains("Compiler encountered"))
      .toSet shouldEqual Set(
      "Test[3:1-3:11]: Redefining atoms is not supported: MyAtom is defined multiple times in this module."
    )
  }

}

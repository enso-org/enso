package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterContext,
  InterpreterException,
  InterpreterTest
}
import org.enso.polyglot.RuntimeOptions
import org.graalvm.polyglot.Context

class OverloadsResolutionErrorTest extends InterpreterTest {
  override def subject: String = "Symbol Overloads"

  override def contextModifiers: Option[Context#Builder => Context#Builder] =
    Some(_.option(RuntimeOptions.STRICT_ERRORS, "true"))

  private def isDiagnosticLine(line: String): Boolean = {
    line.contains(" | ")
  }

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "result in an error at runtime for method overloads" in {
      val code =
        """import Standard.Base.Nothing
          |
          |Nothing.foo = 10
          |Nothing.foo = 20
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics
        .filterNot(isDiagnosticLine)
        .toSet shouldEqual Set(
        "Test:4:1: error: Method overloads are not supported: Nothing.foo is defined multiple times in this module."
      )
    }

    "result in an error at runtime for atom overloads" in {
      val code =
        """
          |type MyAtom
          |type MyAtom
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics
        .filterNot(isDiagnosticLine)
        .toSet shouldEqual Set(
        "Test:3:1: error: Redefining atoms is not supported: MyAtom is defined multiple times in this module."
      )
    }

  }
}

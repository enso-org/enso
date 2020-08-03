package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{
  InterpreterTest,
  InterpreterContext,
  InterpreterException
}
import org.enso.polyglot.RuntimeOptions
import org.graalvm.polyglot.Context

class OverloadsResolutionErrorTest extends InterpreterTest {
  override def subject: String = "Symbol Overloads"

  override def contextModifiers: Context#Builder => Context#Builder =
    _.option(RuntimeOptions.STRICT_ERRORS, "true")

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "result in an error at runtime for methods" in {
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
        .filterNot(_.contains("In module"))
        .toSet shouldEqual Set(
        "Test[3:1-3:13]: Method overloads are not supported: Unit.foo is defined multiple times in this module."
      )
    }

    "result in an error at runtime for atoms" in {
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
        .filterNot(_.contains("In module"))
        .toSet shouldEqual Set(
        "Test[3:1-3:11]: Redefining atoms is not supported: MyAtom is defined multiple times in this module."
      )
    }
  }

}

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
        .filterNot(_.contains("Compiler encountered"))
        .filterNot(_.contains("In module"))
        .toSet shouldEqual Set(
        "Test[4:1-4:16]: Method overloads are not supported: Nothing.foo is defined multiple times in this module."
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
        .filterNot(_.contains("Compiler encountered"))
        .filterNot(_.contains("In module"))
        .toSet shouldEqual Set(
        "Test[3:1-3:11]: Redefining atoms is not supported: MyAtom is defined multiple times in this module."
      )
    }

    "result in an error at runtime for methods overloading atoms" in {
      val code =
        """
          |type Foo
          |foo = 0
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics
        .filterNot(_.contains("Compiler encountered"))
        .filterNot(_.contains("In module"))
        .toSet shouldEqual Set(
        "Test[3:1-3:7]: Method definitions with the same name as atoms are not supported. Method foo clashes with the atom Foo in this module."
      )
    }

  }
}

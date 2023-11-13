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

    "result in an error at runtime for member method overloads" in {
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

    "result in an error at runtime for module method overloads" in {
      val code =
        """bar x y = x + y
          |bar x = x + 10
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics
        .filterNot(isDiagnosticLine)
        .toSet shouldEqual Set(
        "Test:2:1: error: Method overloads are not supported: bar is defined multiple times in this module."
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

    "result in a compiler error for clashing from conversions" in {
      val code =
        """type Foo
          |    Mk_Foo data
          |type Bar
          |    Mk_Bar x
          |
          |Foo.from (that : Bar) = Foo.Mk_Foo that.x+100
          |Foo.from (that : Bar) = Foo.Mk_Foo that.x+200
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics should have length 3
      val line0 =
        "Test:7:1: error: Ambiguous conversion: Foo.from Bar is defined multiple times in this module."
      val line1 = "    7 | Foo.from (that : Bar) = Foo.Mk_Foo that.x+200"
      val line2 = "      | ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      diagnostics shouldEqual List(line0, line1, line2)
    }

    "render the compiler error correctly for clashing multiline from conversions" in {
      val code =
        """type Foo
          |    Mk_Foo data
          |type Bar
          |    Mk_Bar x
          |
          |Foo.from (that : Bar) =
          |    y = that.x+100
          |    Foo.Mk_Foo y
          |Foo.from (that : Bar) =
          |    y = that.x+200
          |    Foo.Mk_Foo y
          |""".stripMargin.linesIterator.mkString("\n")

      the[InterpreterException] thrownBy eval(code) should have message
      "Compilation aborted due to errors."

      val diagnostics = consumeOut
      diagnostics should have length 4
      val line0 =
        "Test:[9:1-11:16]: error: Ambiguous conversion: Foo.from Bar is defined multiple times in this module."
      val line1 = "    9 | Foo.from (that : Bar) ="
      val line2 = "   10 |     y = that.x+200"
      val line3 = "   11 |     Foo.Mk_Foo y"
      diagnostics shouldEqual List(line0, line1, line2, line3)
    }

  }
}

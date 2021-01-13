package org.enso.compiler.test

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

/** Testing the initialization of the builtins IR.
  */
class BuiltinsIrBuilderTest extends InterpreterTest {
  override def subject: String = "Builtins IR Generation"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "initialize the builtins IR from the builtins stub source file" in {
      val code =
        """from Builtins import all
          |
          |main = Debug.eval "1 + 1"
          |""".stripMargin

      println(eval(code))
    }
  }
}

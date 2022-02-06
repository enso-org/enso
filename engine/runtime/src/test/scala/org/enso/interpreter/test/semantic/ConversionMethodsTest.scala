package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class ConversionMethodsTest extends InterpreterTest {
  override def subject: String = "Methods"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "be defined in the global scope and dispatched to" in {
      val code =
        """
          |type Foo foo
          |type Bar bar
          |type Baz baz
          |
          |Foo.from (that:Bar) = Foo that.bar
          |Foo.from (that:Baz) = Foo that.baz
          |
          |main = (Foo.from (Baz 10)).foo + (Foo.from (Bar 20)).foo
          |""".stripMargin
      eval(code) shouldEqual 30
    }
  }
}

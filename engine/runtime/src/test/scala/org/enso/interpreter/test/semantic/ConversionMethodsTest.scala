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
          |type Foo
          |    Mk_Foo foo
          |type Bar
          |    Mk_Bar bar
          |type Baz
          |    Mk_Baz baz
          |
          |Foo.from (that:Bar) = Foo.Mk_Foo that.bar
          |Foo.from (that:Baz) = Foo.Mk_Foo that.baz
          |
          |main = (Foo.from (Baz.Mk_Baz 10)).foo + (Foo.from (Bar.Mk_Bar 20)).foo
          |""".stripMargin
      eval(code) shouldEqual 30
    }
  }
}

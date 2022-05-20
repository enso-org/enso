package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class JavaInteropTest extends InterpreterTest {

  override def subject: String = "Java Interop"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "allow importing classes and calling methods on them" in {
      val code =
        """
          |polyglot java import org.enso.example.TestClass
          |
          |main = TestClass.add 1 2
          |""".stripMargin

      eval(code) shouldEqual 3
    }

    "allow instantiating objects and calling methods on them" in {
      val code =
        """
          |polyglot java import org.enso.example.TestClass
          |
          |main =
          |    instance = TestClass.new (x -> x * 2)
          |    instance.callFunctionAndIncrement 10
          |""".stripMargin
      eval(code) shouldEqual 21
    }
  }
}

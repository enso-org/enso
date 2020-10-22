package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class PolyglotTest extends InterpreterTest {
  override def subject: String = "Polyglot"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "allow calling methods on static objects" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    class = Java.lookup_class "org.enso.example.TestClass"
          |    method = Polyglot.get_member class "add"
          |    Polyglot.execute method (Array.new_2 1 2)
          |""".stripMargin

      eval(code) shouldEqual 3
    }

    "allow instantiating objects and calling methods on them" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    class = Java.lookup_class "org.enso.example.TestClass"
          |    instance = Polyglot.new class (Array.new_1 (x -> x * 2))
          |    Polyglot.invoke instance "callFunctionAndIncrement" (Array.new_1 10)
          |""".stripMargin
      eval(code) shouldEqual 21
    }

    "allow listing available members of an object" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    class = Java.lookup_class "org.enso.example.TestClass"
          |    instance = Polyglot.new class Array.empty
          |    members = Polyglot.get_members instance
          |    IO.println members.length
          |    IO.println (members.at 0)
          |    IO.println (members.at 1)
          |    IO.println (members.at 2)
          |""".stripMargin
      eval(code)
      val count :: methods = consumeOut
      count shouldEqual "3"
      methods.toSet shouldEqual Set(
        "method1",
        "method2",
        "callFunctionAndIncrement"
      )
    }
  }
}

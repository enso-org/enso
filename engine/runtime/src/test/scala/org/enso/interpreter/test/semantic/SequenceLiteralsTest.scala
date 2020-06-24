package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class SequenceLiteralsTest extends InterpreterTest {
  override def subject: String = "Vector Literals"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {
    "create collections, with fields accessible through the Polyglot API" in {
      val code =
        """
          |type My x y
          |
          |main =
          |    vec = [1, "abc", My 1 2]
          |    IO.println (Polyglot.get_array_element vec 0)
          |    IO.println (Polyglot.get_array_element vec 1)
          |    IO.println (Polyglot.get_array_element vec 2)
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("1", "abc", "My 1 2")
    }
  }
}

package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class StringTest extends InterpreterTest {
  "Strings" should "exist in the language and be printable" in {
    val code =
      """
        |@println [@IO, "hello world!"]
        |""".stripMargin

    noException shouldBe thrownBy(eval(code))
    consumeOut shouldEqual List("hello world!")
  }
}

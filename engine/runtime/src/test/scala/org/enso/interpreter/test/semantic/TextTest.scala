package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class TextTest extends InterpreterTest {
  "Single line raw text literals" should "exist in the language" in {
    val code =
      """
        |main = IO.println "hello world!"
        |""".stripMargin

    eval(code)
    consumeOut shouldEqual List("hello world!")
  }

  "Block raw text literals" should "exist in the language" in {
    val code =
      s"""
         |main =
         |    x = $rawTQ
         |        Foo
         |        Bar
         |          Baz
         |
         |    IO.println x
         |""".stripMargin

    eval(code)
    consumeOut shouldEqual List("Foo", "Bar", "  Baz")
  }

  "Raw text literals" should "support escape sequences" in {
    val code =
      """
        |main = IO.println "\"Grzegorz Brzeczyszczykiewicz\""
        |""".stripMargin

    eval(code)
    consumeOut shouldEqual List("\"Grzegorz Brzeczyszczykiewicz\"")
  }
}

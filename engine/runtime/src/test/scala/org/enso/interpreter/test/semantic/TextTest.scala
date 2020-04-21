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

  "String concatenation" should "exist in the language" in {
    val code =
      """
        |main =
        |    h = "Hello, "
        |    w = "World!"
        |    IO.println h+w
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("Hello, World!")
  }

  "Arbitrary structures" should "be auto-convertible to strings with the to_text method" in {
    val code =
      """
        |type My_Type a
        |
        |main =
        |    IO.println 5.to_text
        |    IO.println (My_Type (My_Type 10)).to_text
        |    IO.println "123".to_text
        |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("5", "My_Type (My_Type 10)", "123")
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

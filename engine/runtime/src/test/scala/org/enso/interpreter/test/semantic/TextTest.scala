package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class TextTest extends InterpreterTest {
  override def subject = "Text Library"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "support text creation with single-line literals" in {
      val code =
        """
          |main = IO.println "hello world!"
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("hello world!")
    }

    "support text concatenation" in {
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

    "support converting arbitrary structures to text" in {
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

    "support text creation with raw block literals" in {
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

    "support escape sequences in literals" in {
      val code =
        """
          |main = IO.println "\"Grzegorz Brzeczyszczykiewicz\""
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("\"Grzegorz Brzeczyszczykiewicz\"")
    }

    "support printing to standard error" in {
      val errString = "\"My error string\""
      val resultStr = errString.drop(1).dropRight(1)

      val code =
        s"""
           |main = IO.print_err $errString
           |""".stripMargin

      eval(code)
      consumeErr shouldEqual List(resultStr)
    }

    "support reading from standard input" in {
      val inputString = "foobarbaz"

      val code =
        """
          |main =
          |    IO.readln + " yay!"
          |""".stripMargin

      feedInput(inputString)

      eval(code) shouldEqual "foobarbaz yay!"
    }
  }
}

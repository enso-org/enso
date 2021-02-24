package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class TextTest extends InterpreterTest {
  override def subject = "Text Library"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "support text creation with single-line literals" in {
      val code =
        """from Builtins import all
          |
          |main = IO.println "hello world!"
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("hello world!")
    }

    "support text concatenation" in {
      val code =
        """from Builtins import all
          |
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
        """from Builtins import all
          |
          |type My_Type a
          |
          |main =
          |    IO.println 5
          |    IO.println (My_Type (My_Type 10))
          |    IO.println "123"
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("5", "(My_Type (My_Type 10))", "123")
    }

    "support text creation with raw block literals" in {
      val code =
        s"""from Builtins import all
           |
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
        """from Builtins import all
          |
          |main = IO.println "\"Grzegorz Brzeczyszczykiewicz\""
          |""".stripMargin

      eval(code)
      consumeOut shouldEqual List("\"Grzegorz Brzeczyszczykiewicz\"")
    }

    "support printing to standard error" in {
      val code =
        s"""from Builtins import all
           |
           |main = IO.print_err "My error string"
           |""".stripMargin

      eval(code)
      consumeErr shouldEqual List("My error string")
    }

    "support reading from standard input" in {
      val inputString = "foobarbaz"

      val code =
        """from Builtins import all
          |
          |main =
          |    IO.readln + " yay!"
          |""".stripMargin

      feedInput(inputString)

      eval(code) shouldEqual "foobarbaz yay!"
    }

    "support converting values to display texts" in {
      val code =
        """
          |from Builtins import all
          |
          |main =
          |    IO.println (Cons Nothing Nothing).to_display_text
          |    IO.println (Syntax_Error "foo").to_display_text
          |    IO.println (Type_Error Nothing Text "myvar").to_display_text
          |    IO.println (Compile_Error "error :(").to_display_text
          |    IO.println (Inexhaustive_Pattern_Match_Error 32).to_display_text
          |    IO.println (Arithmetic_Error "cannot frobnicate quaternions").to_display_text
          |    IO.println ((Panic.recover (1 + "foo")).catch_primitive .to_display_text)
          |    IO.println ((Panic.recover (7 1)).catch_primitive .to_display_text)
          |    IO.println (Arity_Error 10 20).to_display_text
          |""".stripMargin
      eval(code)
      consumeOut shouldEqual List(
        "Cons",
        "Syntax error: foo",
        "Type error: expected `myvar` to be Nothing, but got Text.",
        "Compile error: error :(",
        "Inexhaustive pattern match: no branch matches 32 (Integer).",
        "Arithmetic error: cannot frobnicate quaternions",
        "Type error: expected `that` to be Number, but got Text.",
        "Type error: expected a function, but got 7 (Integer).",
        "Wrong number of arguments. Expected 10, but got 20."
      )
    }
  }
}

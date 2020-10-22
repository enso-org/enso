package org.enso.std.test

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class JsonSerializationTest extends InterpreterTest {

  override def subject = "Automatic JSON serialization"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {

    "support strings" in {
      val code =
        """
          |main = "it's a \"string\"" . json_serialize
          |""".stripMargin
      eval(code) shouldEqual "\"it's a \\\"string\\\"\""
    }

    "support nubmers" in {
      val code =
        """
          |main = 1234 . json_serialize
          |""".stripMargin
      eval(code) shouldEqual "1234"
    }

    "support atoms" in {
      val code =
        """from Builtins import all
          |
          |type X a b c
          |
          |main = X 123 "foo" Unit . json_serialize
          |""".stripMargin
      eval(
        code
      ) shouldEqual """{"type":"X","fields":[123,"foo",{"type":"Unit","fields":[]}]}"""
    }

    "support functions" in {
      val code =
        """
          |main = (x -> x).json_serialize
          |""".stripMargin
      eval(code) shouldEqual "null"
    }

    "support nested types" in {
      val code =
        """from Builtins import all
          |
          |main =
          |    test_val = Cons 1 (Cons "\"foo\"" (Cons Unit (Cons (x -> x) Nil)))
          |    test_val.json_serialize
          |""".stripMargin

      val expectedResult =
        """{"type":"Cons","fields":[1,{"type":"Cons","fields":["\"foo\"",{"type":
          |"Cons","fields":[{"type":"Unit","fields":[]},{"type":"Cons","fields":
          |[null,{"type":"Nil","fields":[]}]}]}]}]}""".stripMargin.linesIterator
          .mkString("")

      eval(code) shouldEqual expectedResult
    }
  }
}

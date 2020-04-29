package org.enso.std.test

import org.enso.interpreter.test.InterpreterTest

class JsonSerializationTest extends InterpreterTest {

  "strings" should "be serializable" in {
    val code =
      """
        |main = "it's a \"string\"" . json_serialize
        |""".stripMargin
    eval(code) shouldEqual "\"it's a \\\"string\\\"\""
  }

  "nubmers" should "be serializable" in {
    val code =
      """
        |main = 1234 . json_serialize
        |""".stripMargin
    eval(code) shouldEqual "1234"
  }

  "atoms" should "be serializable" in {
    val code =
      """
        |type X a b c
        |
        |main = X 123 "foo" Unit . json_serialize
        |""".stripMargin
    eval(code) shouldEqual """{"type":"X","fields":[123,"foo",{"type":"Unit","fields":[]}]}"""
  }

  "functions" should "serialize as a null" in {
    val code =
      """
        |main = (x -> x).json_serialize
        |""".stripMargin
    eval(code) shouldEqual "null"
  }

  "nested types" should "serialize recursively" in {
    val code =
      """
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

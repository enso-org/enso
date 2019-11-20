package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class EvalTest extends InterpreterTest {
  "Debug.eval" should "evaluate a string expression" in {
    val code =
      """
        |@{
        |  @eval [@Debug, "@println[@IO, \"foo\"]"]
        |}
        |""".stripMargin
    evalOld(code)
    consumeOut shouldEqual List("foo")
  }

  "Debug.eval" should "have access to the caller scope" in {
    val code =
      """
        |@{
        |  x = "Hello World!";
        |  @eval [@Debug, "@println[@IO, x]"]
        |}
        |""".stripMargin
    evalOld(code)
    consumeOut shouldEqual List("Hello World!")
  }

  "Debug.eval" should "have access to the caller module scope" in {
    val code =
      """
        |type MyType x;
        |
        |@{
        |  x = 10;
        |  @eval [@Debug, "@println[@IO, @MyType[x]]"]
        |}
        |""".stripMargin
    evalOld(code)
    consumeOut shouldEqual List("MyType<10>")
  }

  "Debug.eval" should "return a value usable in the caller scope" in {
    val code =
      """
        |@{
        |  x = 1;
        |  y = 2;
        |  res = @eval [@Debug, "x + y"];
        |  res + 1
        |}
        |""".stripMargin
    evalOld(code) shouldEqual 4
  }

  "Debug.eval" should "work in a recursive setting" in {
    val code =
      """
        |{ |sumTo|
        |  summator = { |acc, current|
        |      @eval [@Debug, "@ifZero [current, acc, @summator [acc + current, current - 1]]"]
        |  };
        |  res = @summator [0, sumTo];
        |  res
        |}
        |""".stripMargin
    val fun = evalOld(code)
    fun.call(100) shouldEqual 5050
  }

  "Debug.eval" should "work inside a thunk passed to another function" in {
    val code =
      """
        |{ |sumTo|
        |  summator = { |acc, current|
        |      @ifZero [current, acc, @eval [@Debug, "@summator [acc + current, current - 1]"]]
        |  };
        |  res = @summator [0, sumTo];
        |  res
        |}
        |""".stripMargin
    val fun = evalOld(code)
    fun.call(100) shouldEqual 5050
  }
}

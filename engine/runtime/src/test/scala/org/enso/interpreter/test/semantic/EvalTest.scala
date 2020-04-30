package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class EvalTest extends InterpreterTest {
  "Debug.eval" should "evaluate a string expression" in {
    val code =
      s"""
         |main =
         |    Debug.eval $rawTQ
         |        IO.println "foo"
         |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("foo")
  }

  "Debug.eval" should "have access to the caller scope" in {
    val code =
      s"""
         |main =
         |    x = "Hello World!"
         |    Debug.eval $rawTQ
         |        IO.println x
         |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("Hello World!")
  }

  "Debug.eval" should "have access to the caller module scope" in {
    val code =
      s"""
         |type MyType x
         |
         |main =
         |    x = 10
         |    Debug.eval $rawTQ
         |        IO.println (MyType x)
         |""".stripMargin
    eval(code)
    consumeOut shouldEqual List("MyType 10")
  }

  "Debug.eval" should "return a value usable in the caller scope" in {
    val code =
      """
        |main =
        |    x = 1
        |    y = 2
        |
        |    res = Debug.eval "x + y"
        |    res + 1
        |""".stripMargin
    eval(code) shouldEqual 4
  }

  "Debug.eval" should "work in a recursive setting" in {
    val code =
      """
        |main =
        |    fn = sumTo ->
        |        summator = acc -> current ->
        |            Debug.eval "ifZero current acc (summator (acc + current) (current - 1))"
        |        summator 0 sumTo
        |    fn 100
        |""".stripMargin
    eval(code) shouldEqual 5050
  }

  "Debug.eval" should "work inside a thunk passed to another function" in {
    val code =
      """
        |main =
        |    fn = sumTo ->
        |        summator = acc -> current ->
        |            ifZero current acc (Debug.eval "summator (acc + current) (current - 1)")
        |
        |        summator 0 sumTo
        |
        |    fn 100
        |""".stripMargin
    eval(code) shouldEqual 5050
  }
}

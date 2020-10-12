package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterTest, InterpreterContext}

class EvalTest extends InterpreterTest {
  override def subject: String = "Debug.eval"

  override def specify(
    implicit interpreterContext: InterpreterContext
  ): Unit = {

    "evaluate a string expression" in {
      val code =
        s"""
           |main =
           |    Debug.eval $rawTQ
           |        IO.println "foo"
           |""".stripMargin
      eval(code)
      consumeOut shouldEqual List("foo")
    }

    "have access to the caller scope" in {
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

    "have access to the caller module scope" in {
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

    "return a value usable in the caller scope" in {
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

    "work in a recursive setting" in {
      val code =
        """
          |main =
          |    fn = sumTo ->
          |        summator = acc -> current ->
          |            Debug.eval "if current == 0 then acc else summator (acc + current) (current - 1)"
          |        summator 0 sumTo
          |    fn 100
          |""".stripMargin
      eval(code) shouldEqual 5050
    }

    "work inside a thunk passed to another function" in {
      val code =
        """
          |main =
          |    fn = sumTo ->
          |        summator = acc -> current ->
          |            if current == 0 then acc else Debug.eval "summator (acc + current) (current - 1)"
          |
          |        summator 0 sumTo
          |
          |    fn 100
          |""".stripMargin
      eval(code) shouldEqual 5050
    }
  }
}

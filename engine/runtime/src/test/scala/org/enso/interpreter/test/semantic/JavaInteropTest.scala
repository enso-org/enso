package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class JavaInteropTest extends InterpreterTest {
  "Java interop" should "allow importing classes and calling methods on them" in {
    val code =
      """
        |polyglot java import org.enso.example.TestClass
        |
        |main = TestClass.add [1, 2]
        |""".stripMargin

    eval(code) shouldEqual 3
  }

  "Java interop" should "allow instantiating objects and calling methods on them" in {
    val code =
      """
        |polyglot java import org.enso.example.TestClass
        |
        |main =
        |    instance = TestClass.new [x -> x * 2]
        |    instance.callFunctionAndIncrement [10]
        |""".stripMargin
    eval(code) shouldEqual 21
  }
}

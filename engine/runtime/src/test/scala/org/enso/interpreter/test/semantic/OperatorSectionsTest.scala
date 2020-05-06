package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class OperatorSectionsTest extends InterpreterTest {

  "Left operator sections" should "work" in {
    val code =
      """
        |main =
        |    f = (1 +)
        |    f 9
        |""".stripMargin

    eval(code) shouldEqual 10
  }

  "Sides operator sections" should "work" in {
    val code =
      """
        |main =
        |    f = (-)
        |    f 1 6
        |""".stripMargin

    eval(code) shouldEqual -5
  }

  "Right operator sections" should "work" in {
    val code =
      """
        |main =
        |    f = (- 10)
        |    f 5
        |""".stripMargin

    eval(code) shouldEqual -5
  }
}

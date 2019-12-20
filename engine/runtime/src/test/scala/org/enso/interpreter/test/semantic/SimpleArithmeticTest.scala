package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.InterpreterTest

class SimpleArithmeticTest extends InterpreterTest {
  "1" should "equal 1" in {
    eval("main = 1") shouldEqual 1
  }

  "1 + 1" should "equal 2" in {
    eval("main = 1 + 1") shouldEqual 2
  }

  "2 + (2 * 2)" should "equal 6" in {
    eval("main = 2 + (2 * 2)") shouldEqual 6
  }

  "2 + 2 * 3" should "equal 8" in {
    eval("main = 2 + 2 * 3") shouldEqual 8
  }

  "2 * 2 / 2" should "equal 2" in {
    eval("main = 2 * 2 / 2") shouldEqual 2
  }
}

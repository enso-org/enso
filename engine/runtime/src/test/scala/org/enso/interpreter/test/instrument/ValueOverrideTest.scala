package org.enso.interpreter.test.instrument
import org.enso.interpreter.test.InterpreterTest

class ValueOverrideTest extends InterpreterTest {
  val subject = "Value override instrument"

  subject should "allow to inject custom values into node execution flow" in {
    val code =
      """
        |main =
        |    x = 123 + 456
        |    y = x * 2
        |    z = y + x
        |    z
        |""".stripMargin

    eval(code) shouldEqual 1737

    val instrument = getValueOverrideInstrument
    instrument.overrideAt("Test.main", 16, 9, 1L.asInstanceOf[AnyRef])
    instrument.overrideAt("Test.main", 34, 5, 10L.asInstanceOf[AnyRef])

    eval(code) shouldEqual 11
  }

  subject should "skip evaluation of side effects when overriding an expression" in {
    val code =
      """
        |foo = arg ->
        |    IO.println "I'm expensive!"
        |    arg + 5
        |
        |bar = arg ->
        |    IO.println "I'm more expensive!"
        |    arg * 5
        |
        |main =
        |    x = 10
        |    y = here.foo x
        |    z = here.bar y
        |    z
        |""".stripMargin

    eval(code) shouldEqual 75
    consumeOut shouldEqual List("I'm expensive!", "I'm more expensive!")

    val instrument = getValueOverrideInstrument
    instrument.overrideAt("Test.main", 148, 10, 15L.asInstanceOf[AnyRef])
    instrument.overrideAt("Test.main", 167, 10, 75L.asInstanceOf[AnyRef])

    eval(code) shouldEqual 75
    consumeOut shouldEqual List()
  }
}

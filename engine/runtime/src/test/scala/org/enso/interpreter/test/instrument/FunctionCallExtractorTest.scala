package org.enso.interpreter.test.instrument
import java.util.function.Consumer

import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.test.InterpreterTest

class FunctionCallExtractorTest extends InterpreterTest {
  val subject = "function call extractor instrument"
  subject should "trigger on function calls and provide function and arguments objects" in {
    val code =
      """
        |foo = x ->
        |    y = x + 1
        |    z = y + 1
        |    r = here.bar z
        |    r
        |
        |bar = y ->
        |    z = y + 5
        |    w = z * 5
        |    w
        |
        |main =
        |    y = here.foo 1234
        |    z = here.bar y
        |    IO.println z
        |    0
        |""".stripMargin
    var result: List[(String, List[String])] = List()

    val instrument = getFunctionCallExtractorInstrument
    val listener: Consumer[FunctionCall] = { x =>
      result :+= (
        (
          x.getFunction.getCallTarget.getRootNode.getName,
          x.getArguments.toList.map(_.toString)
        )
      )
    }
    instrument.bindTo("Test.main", 127, 13, listener)
    instrument.bindTo("Test.main", 149, 10, listener)
    instrument.bindTo("Test.main", 164, 12, listener)
    eval(code)
    result shouldEqual List(
      ("Test.foo", List("Test", "1234")),
      ("Test.bar", List("Test", "6205")),
      ("IO.println", List("IO", "31050"))
    )
  }

  subject should "work for recursive calls" in {
    val code =
      """
        |main = arg ->
        |    x = arg - 1
        |    y = x.ifZero 0 (here.main x)
        |    z = y + arg
        |    z
        |""".stripMargin
    var yVal: Option[(String, List[String])] = None
    getFunctionCallExtractorInstrument.bindTo("Test.main", 51, 11, { y =>
      yVal = Some(
        (
          y.getFunction.getCallTarget.getRootNode.getName,
          y.getArguments.toList.map(_.toString)
        )
      )
    })
    getMain(code).execute(5L.asInstanceOf[AnyRef])
    yVal shouldEqual Some(("Test.main", List("Test", "4")))
  }

}

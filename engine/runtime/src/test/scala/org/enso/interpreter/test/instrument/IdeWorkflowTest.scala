package org.enso.interpreter.test.instrument
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode.FunctionCall
import org.enso.interpreter.test.InterpreterTest
import org.graalvm.polyglot.Value

class IdeWorkflowTest extends InterpreterTest {

  def executeCallData(callData: FunctionCall): Value = {
    ctx.asValue(callData).execute()
  }

  "instruments" should "allow to execute functions, enter their call-sites and display values" in {
    val code =
      """
        |foo = arg ->
        |    a = arg * State.get
        |    b = a - 1
        |    a + b
        |
        |bar = arg ->
        |    x = arg - 1
        |    rec = x.ifZero 1 (arg + here.bar x)
        |    rec
        |
        |main = arg ->
        |    a = here.bar arg
        |    State.put 3
        |    b = here.foo a
        |    b
        |
        |""".stripMargin

    val module    = executionContext.evalModule(code, "Test")
    val assocCons = module.getAssociatedConstructor
    val main      = module.getMethod(assocCons, "main")

    // 1st breadcrumb: [main]
    var mainValues: Map[String, Any] = Map()
    getValueExtractorInstrument.bindTo("Test.main", 163, 12, { a =>
      mainValues += "a" -> a
    })
    getValueExtractorInstrument.bindTo("Test.main", 200, 10, { b =>
      mainValues += "b" -> b
    })
    var bar0FunctionCall: Option[FunctionCall] = None
    getFunctionCallExtractorInstrument.bindTo("Test.main", 163, 12, { a =>
      bar0FunctionCall = Some(a)
    })
    var fooFunctionCall: Option[FunctionCall] = None
    getFunctionCallExtractorInstrument.bindTo("Test.main", 200, 10, { b =>
      fooFunctionCall = Some(b)
    })
    main.execute(assocCons, 5L.asInstanceOf[AnyRef])

    mainValues shouldEqual Map("a" -> 15, "b" -> 89)

    // 2nd breadcrumb: [main > bar]
    var bar0Values: Map[String, Any] = Map()
    getValueExtractorInstrument.bindTo("Test.bar", 84, 7, { x =>
      bar0Values += "x" -> x
    })
    getValueExtractorInstrument.bindTo("Test.bar", 102, 29, { rec =>
      bar0Values += "rec" -> rec
    })

    var bar1FunctionCall: Option[FunctionCall] = None
    getFunctionCallExtractorInstrument.bindTo("Test.bar", 120, 10, { rec =>
      bar1FunctionCall = Some(rec)
    })

    executeCallData(bar0FunctionCall.get)
    bar0Values shouldEqual Map("x" -> 4, "rec" -> 15)

    // 3rd breadcrumb: [main > bar > bar]
    var bar1Values: Map[String, Any] = Map()
    getValueExtractorInstrument.bindTo("Test.bar", 84, 7, { x =>
      bar1Values += "x" -> x
    })
    getValueExtractorInstrument.bindTo("Test.bar", 102, 29, { rec =>
      bar1Values += "rec" -> rec
    })

    executeCallData(bar1FunctionCall.get)
    bar1Values shouldEqual Map("x" -> 3, "rec" -> 10)

    // 4th breadcrumb: [main > foo]
    var fooValues: Map[String, Any] = Map()
    getValueExtractorInstrument.bindTo("Test.foo", 22, 15, { a =>
      fooValues += "a" -> a
    })
    getValueExtractorInstrument.bindTo("Test.foo", 46, 5, { b =>
      fooValues += "b" -> b
    })

    executeCallData(fooFunctionCall.get)
    fooValues shouldEqual Map("a" -> 45, "b" -> 44)

  }

  "instruments" should "allow to get executed values and use them in caches" in {
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

    val module    = executionContext.evalModule(code, "Test")
    val assocCons = module.getAssociatedConstructor
    val main      = module.getMethod(assocCons, "main")

    val extractInstrument = getValueExtractorInstrument
    var yVal: AnyRef      = null
    var zVal: AnyRef      = null
    extractInstrument.bindTo("Test.main", 148, 10, { y =>
      yVal = y
    })
    extractInstrument.bindTo("Test.main", 167, 10, { z =>
      zVal = z
    })

    main.execute(assocCons.newInstance()) shouldEqual 75
    consumeOut shouldEqual List("I'm expensive!", "I'm more expensive!")

    val overrideInstrument = getValueOverrideInstrument
    overrideInstrument.overrideAt("Test.main", 148, 10, yVal)
    overrideInstrument.overrideAt("Test.main", 167, 10, zVal)

    main.execute(assocCons.newInstance()) shouldEqual 75
    consumeOut shouldEqual List()
  }
}

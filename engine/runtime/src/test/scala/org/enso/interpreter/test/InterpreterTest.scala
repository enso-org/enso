package org.enso.interpreter.test

import java.io.ByteArrayOutputStream

import com.oracle.truffle.api.instrumentation.EventBinding
import org.enso.interpreter.instrument.{
  FunctionCallExtractorInstrument,
  ReplDebuggerInstrument,
  ValueExtractorInstrument,
  ValueOverrideInstrument
}
import org.enso.interpreter.test.CodeLocationsTestInstrument.LocationsEventListener
import org.enso.polyglot.{ExecutionContext, Function, LanguageInfo}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.{Assertions, FlatSpec, Matchers}

case class LocationsInstrumenter(instrument: CodeLocationsTestInstrument) {
  var bindings: List[EventBinding[LocationsEventListener]] = List()

  def assertNodeExists(start: Int, length: Int, kind: Class[_]): Unit =
    bindings ::= instrument.bindTo(start, length, kind)

  def verifyResults(): Unit = {
    bindings.foreach { binding =>
      val listener = binding.getElement
      if (!listener.isSuccessful) {
        Assertions.fail(
          s"Node of type ${listener.getType.getSimpleName} at position ${listener.getStart} with length ${listener.getLength} was not found."
        )
      }
    }
  }

  def close(): Unit = {
    bindings.foreach(_.dispose)
  }
}

trait InterpreterRunner {

  implicit class RichValue(value: Value) {
    def call(l: Long*): Value =
      InterpreterException.rethrowPolyglot(
        value.execute(l.map(_.asInstanceOf[AnyRef]): _*)
      )
  }
  val output           = new ByteArrayOutputStream()
  val ctx              = Context.newBuilder(LanguageInfo.ID).out(output).build()
  val executionContext = new ExecutionContext(ctx)

  def withLocationsInstrumenter(test: LocationsInstrumenter => Unit): Unit = {
    val instrument = ctx.getEngine.getInstruments
      .get(CodeLocationsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeLocationsTestInstrument])
    val instrumenter = LocationsInstrumenter(instrument)
    test(instrumenter)
    instrumenter.verifyResults()
    instrumenter.close()
  }

  case class MainMethod(mainConstructor: Value, mainFunction: Function) {
    def execute(args: AnyRef*): Value =
      InterpreterException.rethrowPolyglot(
        mainFunction.execute(mainConstructor +: args: _*)
      )
  }

  def getMain(code: String): MainMethod = {
    output.reset()
    val module = InterpreterException.rethrowPolyglot(
      executionContext.evalModule(code, "Test")
    )
    val assocCons    = module.getAssociatedConstructor
    val mainFunction = module.getMethod(assocCons, "main")
    MainMethod(assocCons, mainFunction)
  }

  def eval(
    code: String
  ): Value = {
    val main = getMain(code)
    InterpreterException.rethrowPolyglot(
      main.mainFunction.execute(main.mainConstructor)
    )
  }

  def consumeOut: List[String] = {
    val result = output.toString
    output.reset()
    result.lines.toList
  }

  def getReplInstrument: ReplDebuggerInstrument = {
    ctx.getEngine.getInstruments
      .get(ReplDebuggerInstrument.INSTRUMENT_ID)
      .lookup(classOf[ReplDebuggerInstrument])
  }

  def getValueExtractorInstrument: ValueExtractorInstrument = {
    ctx.getEngine.getInstruments
      .get(ValueExtractorInstrument.INSTRUMENT_ID)
      .lookup(classOf[ValueExtractorInstrument])
  }

  def getFunctionCallExtractorInstrument: FunctionCallExtractorInstrument = {
    ctx.getEngine.getInstruments
      .get(FunctionCallExtractorInstrument.INSTRUMENT_ID)
      .lookup(classOf[FunctionCallExtractorInstrument])
  }

  def getValueOverrideInstrument: ValueOverrideInstrument = {
    ctx.getEngine.getInstruments
      .get(ValueOverrideInstrument.INSTRUMENT_ID)
      .lookup(classOf[ValueOverrideInstrument])
  }

  // For Enso raw text blocks inside scala multiline strings
  val rawTQ = "\"\"\""
}

trait InterpreterTest
    extends FlatSpec
    with Matchers
    with InterpreterRunner
    with ValueEquality

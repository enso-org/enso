package org.enso.interpreter.test

import java.io.{ByteArrayOutputStream, StringReader}

import com.oracle.truffle.api.instrumentation.EventBinding
import org.enso.interpreter.Constants
import org.graalvm.polyglot.{Context, Source, Value}
import org.enso.interpreter.instrument.{
  ReplDebuggerInstrument,
  ValueExtractorInstrument
}
import org.enso.interpreter.test.CodeLocationsTestInstrument.LocationsEventListener
import org.scalatest.{Assertions, FlatSpec, Matchers}

trait InterpreterRunner {
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

  implicit class RichValue(value: Value) {
    def call(l: Long*): Value =
      InterpreterException.rethrowPolyglot(
        value.execute(l.map(_.asInstanceOf[AnyRef]): _*)
      )
  }
  val output = new ByteArrayOutputStream()
  val ctx    = Context.newBuilder(Constants.LANGUAGE_ID).out(output).build()

  def withLocationsInstrumenter(test: LocationsInstrumenter => Unit): Unit = {
    val instrument = ctx.getEngine.getInstruments
      .get(CodeLocationsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeLocationsTestInstrument])
    val instrumenter = LocationsInstrumenter(instrument)
    test(instrumenter)
    instrumenter.verifyResults()
    instrumenter.close()
  }

  case class MainMethod(mainConstructor: Value, mainFunction: Value) {
    def execute(args: AnyRef*): Value =
      InterpreterException.rethrowPolyglot(
        mainFunction.execute(mainConstructor +: args: _*)
      )
  }

  def getMain(code: String): MainMethod = {
    output.reset()
    val source = Source
      .newBuilder(Constants.LANGUAGE_ID, code, "Test")
      .build()

    val module       = InterpreterException.rethrowPolyglot(ctx.eval(source))
    val assocCons    = module.invokeMember("get_associated_constructor")
    val mainFunction = module.invokeMember("get_method", assocCons, "main")
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

  // For Enso raw text blocks inside scala multiline strings
  val rawTQ = "\"\"\""
}

trait InterpreterTest
    extends FlatSpec
    with Matchers
    with InterpreterRunner
    with ValueEquality

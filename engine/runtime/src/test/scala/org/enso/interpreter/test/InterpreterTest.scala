package org.enso.interpreter.test

import java.io.ByteArrayOutputStream
import java.util.UUID

import com.oracle.truffle.api.instrumentation.EventBinding
import org.enso.interpreter.instrument.{
  FunctionCallExtractorInstrument,
  ReplDebuggerInstrument,
  ValueExtractorInstrument
}
import org.enso.interpreter.test.CodeIdsTestInstrument.IdEventListener
import org.enso.interpreter.test.CodeLocationsTestInstrument.LocationsEventListener
import org.enso.polyglot.{Function, LanguageInfo, PolyglotContext}
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.Assertions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

case class IdsInstrumenter(instrument: CodeIdsTestInstrument) {
  var bindings: List[EventBinding[IdEventListener]] = List()

  def assertNodeExists(id: UUID, result: String): Unit =
    bindings ::= instrument.bindTo(id, result)

  def assertNodeExistsTail(id: UUID): Unit =
    bindings ::= instrument.bindToTailCall(id)

  def verifyResults(): Unit = {
    bindings.foreach { binding =>
      val listener = binding.getElement
      if (!listener.isSuccessful) {
        Assertions.fail(
          s"Node with id ${listener.getId} does not exist or did not return the correct value."
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
  val output = new ByteArrayOutputStream()
  val ctx = Context
    .newBuilder(LanguageInfo.ID)
    .allowExperimentalOptions(true)
    .allowAllAccess(true)
    .out(output)
    .build()
  lazy val executionContext = new PolyglotContext(ctx)

  def withLocationsInstrumenter(test: LocationsInstrumenter => Unit): Unit = {
    val instrument = ctx.getEngine.getInstruments
      .get(CodeLocationsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeLocationsTestInstrument])
    val instrumenter = LocationsInstrumenter(instrument)
    test(instrumenter)
    instrumenter.verifyResults()
    instrumenter.close()
  }

  def withIdsInstrumenter(test: IdsInstrumenter => Unit): Unit = {
    val instrument = ctx.getEngine.getInstruments
      .get(CodeIdsTestInstrument.INSTRUMENT_ID)
      .lookup(classOf[CodeIdsTestInstrument])
    val instrumenter = IdsInstrumenter(instrument)
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
    InterpreterException.rethrowPolyglot {
      val main = getMain(code)
      main.mainFunction.execute(main.mainConstructor)
    }
  }

  def consumeOut: List[String] = {
    val result = output.toString
    output.reset()
    result.linesIterator.toList
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

  // For Enso raw text blocks inside scala multiline strings
  val rawTQ = "\"\"\""
}

trait InterpreterTest
    extends AnyFlatSpec
    with Matchers
    with InterpreterRunner
    with ValueEquality

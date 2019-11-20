package org.enso.interpreter.test

import java.io.{ByteArrayOutputStream, StringReader}

import org.enso.interpreter.Constants
import org.graalvm.polyglot.{Context, Source, Value}
import org.enso.interpreter.instrument.ReplDebuggerInstrument
import org.scalatest.{FlatSpec, Matchers}

trait InterpreterRunner {
  implicit class RichValue(value: Value) {
    def call(l: Long*): Value =
      InterpreterException.rethrowPolyglot(
        value.execute(l.map(_.asInstanceOf[AnyRef]): _*)
      )
  }
  val output = new ByteArrayOutputStream()
  val ctx    = Context.newBuilder(Constants.LANGUAGE_ID).out(output).build()

  def evalGeneric(code: String, mimeType: String): Value = {
    output.reset()

    val source = Source
      .newBuilder(Constants.LANGUAGE_ID, new StringReader(code), "test")
      .mimeType(mimeType)
      .build()

    InterpreterException.rethrowPolyglot(ctx.eval(source))
  }

  def eval(code: String): Value = {
    evalGeneric(code, Constants.MIME_TYPE)
  }

  def evalOld(code: String): Value = {
    evalGeneric(code, Constants.Debug.MIME_TYPE)
  }

  def consumeOut: List[String] = {
    val result = output.toString
    output.reset()
    result.lines.toList
  }

  def parse(code: String): Value =
    InterpreterException.rethrowPolyglot(evalOld(code))

  def getReplInstrument: ReplDebuggerInstrument = {
    ctx.getEngine.getInstruments
      .get(ReplDebuggerInstrument.INSTRUMENT_ID)
      .lookup(classOf[ReplDebuggerInstrument])
  }
}

trait InterpreterTest
    extends FlatSpec
    with Matchers
    with InterpreterRunner
    with ValueEquality

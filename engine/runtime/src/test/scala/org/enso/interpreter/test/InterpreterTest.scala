package org.enso.interpreter.test

import java.io.ByteArrayOutputStream

import org.enso.interpreter.Constants
import org.graalvm.polyglot.{Context, Value}
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

  def eval(code: String): Value = {
    output.reset()
    InterpreterException.rethrowPolyglot(ctx.eval(Constants.LANGUAGE_ID, code))
  }

  def consumeOut: List[String] = {
    val result = output.toString
    output.reset()
    result.lines.toList
  }

  def parse(code: String): Value =
    InterpreterException.rethrowPolyglot(eval(code))
}

trait InterpreterTest
    extends FlatSpec
    with Matchers
    with InterpreterRunner
    with ValueEquality

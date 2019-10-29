package org.enso.interpreter.test

import org.enso.interpreter.AstGlobalScope
import org.enso.interpreter.Constants
import org.enso.interpreter.EnsoParser
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.PolyglotException
import org.graalvm.polyglot.Value
import java.io.ByteArrayOutputStream

import org.scalactic.Equality
import org.scalatest.FlatSpec
import org.scalatest.Matchers

trait LanguageRunner {

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

  def parse(code: String): AstGlobalScope =
    new EnsoParser().parseEnso(code)

  implicit def toPolyglotException(
    interpreterException: InterpreterException
  ): PolyglotException = interpreterException.polyglotException
}

trait ValueEquality {
  implicit val valueEquality: Equality[Value] = (a: Value, b: Any) =>
    b match {
      case _: Long => a.isNumber && a.fitsInLong && a.asLong == b
      case _: Int  => a.isNumber && a.fitsInInt && a.asInt == b
      case _       => false
    }
}

trait LanguageTest
    extends FlatSpec
    with Matchers
    with LanguageRunner
    with ValueEquality

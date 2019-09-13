package org.enso.interpreter.test.semantic

import org.enso.interpreter.{AstGlobalScope, Constants, EnsoParser}
import org.graalvm.polyglot.{Context, Value}
import org.scalactic.Equality
import org.scalatest.{FlatSpec, Matchers}

trait LanguageRunner {
  implicit class RichValue(value: Value) {
    def call(l: Long*): Value = value.execute(l.map(_.asInstanceOf[AnyRef]): _*)
  }
  val ctx = Context.newBuilder(Constants.LANGUAGE_ID).build()
  def eval(code: String): Value = ctx.eval(Constants.LANGUAGE_ID, code)

  def parse(code: String): AstGlobalScope =
    new EnsoParser().parseEnso(code)
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

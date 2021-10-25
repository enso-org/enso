package org.enso.interpreter.test

import org.graalvm.polyglot.Value
import org.scalactic.Equality

trait ValueEquality {
  implicit val valueEquality: Equality[Value] = (a: Value, b: Any) =>
    b match {
      case _: Long   => a.isNumber && a.fitsInLong && a.asLong == b
      case _: Int    => a.isNumber && a.fitsInInt && a.asInt == b
      case _: String => a.isString && a.asString == b
      case _: Value  => a == b
      case _         => false
    }
}

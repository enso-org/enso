package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named

sealed trait DiagnosticType

object DiagnosticType {
  @named("diagnosticTypeError")
  case object Error extends DiagnosticType

  @named("diagnosticTypeWarning")
  case object Warning extends DiagnosticType
}

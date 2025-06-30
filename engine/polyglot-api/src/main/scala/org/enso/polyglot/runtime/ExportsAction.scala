package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named

sealed trait ExportsAction
object ExportsAction {
  @named("exportsActionAdd")
  case class Add() extends ExportsAction
  @named("exportsActionRemove")
  case class Remove() extends ExportsAction
}

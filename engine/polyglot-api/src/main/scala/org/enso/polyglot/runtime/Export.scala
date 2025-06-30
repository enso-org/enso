package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named

sealed trait Export {
  def module: String
}
object Export {

  /** Qualified module re-export.
    *
    * @param module the module name that exports the given module
    * @param alias new module name if the module was renamed in the export
    * clause
    */
  @named("exportQualified")
  case class Qualified(module: String, alias: Option[String]) extends Export

  /** Unqualified module export.
    *
    * @param module the module name that exports the given module
    */
  @named("exportUnqualified")
  case class Unqualified(module: String) extends Export
}

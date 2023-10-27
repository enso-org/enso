package org.enso.compiler.core.ir.module

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{Expression, IdentifiedLocation}

/** A representation of constructs that can only occur in the top-level
  * module scope
  */
trait Scope extends IR {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Scope

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Scope

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Scope
}

package org.enso.compiler

import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}

/** A type containing the information about the execution context for an inline
  * expression.
  *
  * @param localScope the local scope in which the expression is being executed
  * @param moduleScope the module scope in which the expression is being
  *                    executed
  * @param isInTailPosition whether or not the inline expression occurs in tail
  *                         position ([[None]] indicates no information)
  */
case class InlineContext(
  localScope: Option[LocalScope]    = None,
  moduleScope: Option[ModuleScope]  = None,
  isInTailPosition: Option[Boolean] = None
)
object InlineContext {

  /** Implements a null-safe conversion from nullable objects to Scala's option
    * internally.
    *
    * @param localScope the local scope instance
    * @param moduleScope the module scope instance
    * @param isInTailPosition whether or not the inline expression occurs in a
    *                         tail position
    * @return the [[InlineContext]] instance corresponding to the arguments
    */
  def fromJava(
    localScope: LocalScope,
    moduleScope: ModuleScope,
    isInTailPosition: Boolean
  ): InlineContext = {
    InlineContext(
      Option(localScope),
      Option(moduleScope),
      Option(isInTailPosition)
    )
  }
}

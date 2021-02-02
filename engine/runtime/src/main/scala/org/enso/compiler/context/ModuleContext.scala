package org.enso.compiler.context

import org.enso.compiler.pass.PassConfiguration
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.scope.ModuleScope

/** A type containing the information about the execution context for a module.
  *
  * @param ensoContext the Enso language context
  * @param moduleScope the current module scope
  * @param freshNameSupply the compiler's supply of fresh names
  * @param passConfiguration the pass configuration
  */
case class ModuleContext(
  ensoContext: Option[Context]                 = None,
  moduleScope: Option[ModuleScope]             = None,
  freshNameSupply: Option[FreshNameSupply]     = None,
  passConfiguration: Option[PassConfiguration] = None
)

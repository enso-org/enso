package org.enso.compiler.context

import org.enso.compiler.pass.PassConfiguration

/** A type containing the information about the execution context for a module.
  *
  * @param freshNameSupply the compiler's supply of fresh names
  * @param passConfiguration the pass configuration
  */
case class ModuleContext(
  freshNameSupply: Option[FreshNameSupply]     = None,
  passConfiguration: Option[PassConfiguration] = None
)

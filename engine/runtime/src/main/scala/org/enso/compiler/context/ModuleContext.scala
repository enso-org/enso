package org.enso.compiler.context

import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassConfiguration
import org.enso.interpreter.runtime.Module

/** A type containing the information about the execution context for a module.
  *
  * @param module the current module scope
  * @param freshNameSupply the compiler's supply of fresh names
  * @param passConfiguration the pass configuration
  * @param compilerConfig the compiler configuration
  */
case class ModuleContext(
  module: Module,
  freshNameSupply: Option[FreshNameSupply]     = None,
  passConfiguration: Option[PassConfiguration] = None,
  compilerConfig: CompilerConfig
)

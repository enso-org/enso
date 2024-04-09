package org.enso.compiler.context

import org.enso.compiler.PackageRepository
import org.enso.compiler.context.LocalScope
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassConfiguration

/** A type containing the information about the execution context for an inline
  * expression.
  *
  * @param module the module in which the expression is being executed
  * @param compilerConfig the compiler configuration
  * @param localScope the local scope in which the expression is being executed
  * @param isInTailPosition whether or not the inline expression occurs in tail
  *                         position ([[None]] indicates no information)
  * @param freshNameSupply the compiler's supply of fresh names
  * @param passConfiguration the pass configuration
  * @param pkgRepo the compiler's package repository
  */
case class InlineContext(
  private val module: ModuleContext,
  compilerConfig: CompilerConfig,
  localScope: Option[LocalScope]               = None,
  isInTailPosition: Option[Boolean]            = None,
  freshNameSupply: Option[FreshNameSupply]     = None,
  passConfiguration: Option[PassConfiguration] = None,
  pkgRepo: Option[PackageRepository]           = None
) {
  def bindingsAnalysis() = module.bindingsAnalysis()
}
object InlineContext {

  /** Implements a null-safe conversion from nullable objects to Scala's option
    * internally.
    *
    * @param localScope the local scope instance
    * @param module the module defining the context
    * @param isInTailPosition whether or not the inline expression occurs in a
    *                         tail position
    * @param compilerConfig the compiler configuration
    * @param pkgRepo the compiler's package repository
    * @return the [[InlineContext]] instance corresponding to the arguments
    */
  def fromJava(
    localScope: LocalScope,
    module: CompilerContext.Module,
    isInTailPosition: Option[Boolean],
    compilerConfig: CompilerConfig,
    pkgRepo: Option[PackageRepository]
  ): InlineContext = {
    InlineContext(
      localScope       = Option(localScope),
      module           = ModuleContext(module, compilerConfig),
      isInTailPosition = isInTailPosition,
      compilerConfig   = compilerConfig,
      pkgRepo          = pkgRepo
    )
  }

  /** Transform a module context into an inline context, retaining the useful
    * information.
    *
    * @param moduleContext the module context
    * @return an inline context wrapping the same data as `moduleContext`
    */
  def fromModuleContext(moduleContext: ModuleContext): InlineContext = {
    InlineContext(
      localScope        = None,
      module            = moduleContext,
      isInTailPosition  = None,
      freshNameSupply   = moduleContext.freshNameSupply,
      passConfiguration = moduleContext.passConfiguration,
      compilerConfig    = moduleContext.compilerConfig,
      pkgRepo           = moduleContext.pkgRepo
    )
  }
}

package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedConstructor, ResolvedMethod}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}
import org.enso.interpreter.Constants

import scala.annotation.unused

/** Resolves and desugars referent name occurences in non-pattern contexts.
  *
  * 1. Attaches resolution metadata to encountered constructors, modules,
  *    and polygot symbols.
  * 2. Desugars encountered method references into proper applications.
  * 3. Resolves qualified calls to constructors, i.e. a call of the form
  *    `KnownModule.consName a b c` is transformed into `KnownCons a b c`,
  *    if `consName` refers to a constructor and `KnownModule` was successfully
  *    resolved to a module.
  */
case object TypeNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.TypeResolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq()

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    ???
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    val scopeMap = inlineContext.module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis on the module"
    )
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "No fresh name supply passed to UppercaseNames resolver."
      )
    )
    ???
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}

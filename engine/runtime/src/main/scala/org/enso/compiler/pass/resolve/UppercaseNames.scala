package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, BindingAnalysis}

case object UppercaseNames extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = BindingsMap.Resolution

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] =
    Seq(AliasAnalysis, BindingAnalysis)

  /** The passes that are invalidated by running this pass. */
  override val invalidatedPasses: Seq[IRPass] = Seq(AliasAnalysis)

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
    val scopeMap = ir.unsafeGetMetadata(
      BindingAnalysis,
      "No binding analysis on the module"
    )
    ir.mapExpressions(processExpression(_, scopeMap))
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
    processExpression(ir, scopeMap)
  }

  private def processExpression(
    ir: IR.Expression,
    bindings: BindingsMap
  ): IR.Expression =
    ir.transformExpressions {
      case lit: IR.Name.Literal =>
        if (lit.isReferant) {
          val aliasInfo = lit
            .unsafeGetMetadata(
              AliasAnalysis,
              "no alias analysis info on a name"
            )
            .unsafeAs[AliasAnalysis.Info.Occurrence]
          val defLink = aliasInfo.graph.defLinkFor(aliasInfo.id)
          if (defLink.isDefined) {
            lit
          } else {
            val resolution = bindings.resolveUppercaseName(lit.name)
            resolution match {
              case Left(error) =>
                IR.Error.Resolution(
                  lit,
                  IR.Error.Resolution.ResolverError(error)
                )
              case Right(value) =>
                lit.updateMetadata(this -->> BindingsMap.Resolution(value))
            }
          }
        } else { lit }
    }
}

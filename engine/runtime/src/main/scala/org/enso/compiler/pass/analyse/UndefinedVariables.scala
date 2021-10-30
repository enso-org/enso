package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass

import scala.annotation.unused

/** Reports errors for local variables that are not linked to a definition
  *  after Alias Analysis.
  */
case object UndefinedVariables extends IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  override type Metadata = IRPass.Metadata.Empty

  /** The type of configuration for the pass. */
  override type Config = IRPass.Configuration.Default

  /** The passes that this pass depends _directly_ on to run. */
  override val precursorPasses: Seq[IRPass] = Seq(AliasAnalysis)

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
  ): IR.Module = ir.mapExpressions(analyseExpression)

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
  ): IR.Expression = analyseExpression(ir)

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  private def analyseExpression(ir: IR.Expression): IR.Expression =
    ir.transformExpressions { case name: IR.Name.Literal =>
      if (name.isVariable) {
        val occ = name
          .unsafeGetMetadata(
            AliasAnalysis,
            "no alias analysis info on a literal name"
          )
          .unsafeAs[AliasAnalysis.Info.Occurrence]
        occ.graph.defLinkFor(occ.id) match {
          case Some(_) => name
          case None =>
            val errorResolutionNode =
              IR.Error.Resolution(name, IR.Error.Resolution.VariableNotInScope)
            errorResolutionNode.updateMetadata(AliasAnalysis -->> occ)
        }
      } else { name }

    }
}

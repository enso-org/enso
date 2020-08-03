package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis
}

/** This pass converts usages of operators to calls to standard functions.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object OperatorToFunction extends IRPass {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    GenerateMethodBodies,
    SectionsToBinOp
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis
  )

  /** Executes the conversion pass.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module =
    ir.mapExpressions(runExpression(_, new InlineContext(moduleContext.module)))

  /** Executes the conversion pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression =
    ir.transformExpressions {
      case IR.Application.Operator.Binary(l, op, r, loc, passData, diag) =>
        IR.Application.Prefix(
          op,
          List(
            l.mapExpressions(runExpression(_, inlineContext)),
            r.mapExpressions(runExpression(_, inlineContext))
          ),
          hasDefaultsSuspended = false,
          loc,
          passData,
          diag
        )
    }
}

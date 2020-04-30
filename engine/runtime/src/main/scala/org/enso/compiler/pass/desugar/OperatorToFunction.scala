package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass converts usages of operators to calls to standard functions. */
case object OperatorToFunction extends IRPass {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IR.Metadata.Empty

  override type Config = IRPass.Configuration.Default

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
    ir.transformExpressions({
      case x => runExpression(x, new InlineContext)
    })

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
      case IR.Application.Operator.Binary(l, op, r, loc, passData) =>
        IR.Application.Prefix(
          op,
          List(
            IR.CallArgument
              .Specified(None, runExpression(l, inlineContext), l.location),
            IR.CallArgument
              .Specified(None, runExpression(r, inlineContext), r.location)
          ),
          hasDefaultsSuspended = false,
          loc,
          passData
        )
    }
}

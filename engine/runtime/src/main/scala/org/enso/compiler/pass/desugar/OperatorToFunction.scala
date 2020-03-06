package org.enso.compiler.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass converts usages of operators to calls to standard functions. */
case object OperatorToFunction extends IRPass {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IR.Metadata.Empty

  /** Executes the conversion pass.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module =
    ir.transformExpressions({ case x => runExpression(x) })

  /** Executes the conversion pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(ir: IR.Expression): IR.Expression =
    ir.transformExpressions {
      case IR.Application.Operator.Binary(l, op, r, loc, passData) =>
        IR.Application.Prefix(
          op,
          List(
            IR.CallArgument.Specified(None, runExpression(l), l.location),
            IR.CallArgument.Specified(None, runExpression(r), r.location)
          ),
          hasDefaultsSuspended = false,
          loc,
          passData
        )
    }
}

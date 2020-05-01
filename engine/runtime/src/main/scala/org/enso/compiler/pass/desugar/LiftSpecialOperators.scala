package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass lifts any special operators (ones reserved by the language
  * implementation) into their own special IR constructs.
  */
case object LiftSpecialOperators extends IRPass {

  /** A desugaring pass does not output any data. */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module =
    ir.transformExpressions({
      case x => runExpression(x, new InlineContext)
    })

  /** Executes the lifting pass in an inline context.
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
    ir.transformExpressions({
      case IR.Application.Operator.Binary(l, op, r, loc, meta, _) =>
        op.name match {
          case IR.Type.Ascription.name =>
            IR.Type.Ascription(
              runExpression(l, inlineContext),
              runExpression(r, inlineContext),
              loc,
              meta
            )
          case IR.Type.Set.Subsumption.name =>
            IR.Type.Set
              .Subsumption(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case IR.Type.Set.Equality.name =>
            IR.Type.Set
              .Equality(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case IR.Type.Set.Concat.name =>
            IR.Type.Set
              .Concat(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case IR.Type.Set.Union.name =>
            IR.Type.Set
              .Union(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case IR.Type.Set.Intersection.name =>
            IR.Type.Set
              .Intersection(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case IR.Type.Set.Subtraction.name =>
            IR.Type.Set
              .Subtraction(
                runExpression(l, inlineContext),
                runExpression(r, inlineContext),
                loc,
                meta
              )
          case _ =>
            IR.Application.Operator
              .Binary(
                runExpression(l, inlineContext),
                op,
                runExpression(r, inlineContext),
                loc,
                meta
              )
        }
    })

}

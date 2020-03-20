package org.enso.compiler.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}

import scala.annotation.unused

/** This pass lifts any special operators (ones reserved by the language
  * implementation) into their own special IR constructs.
  */
case object LiftSpecialOperators extends IRPass {

  /** A desugaring pass does not output any data. */
  override type Metadata = IR.Metadata.Empty

  /** Executes the lifting pass on a module.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module =
    ir.transformExpressions({ case x => runExpression(x) })

  /** Executes the lifting pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    @unused localScope: Option[LocalScope]   = None,
    @unused moduleScope: Option[ModuleScope] = None
  ): IR.Expression =
    ir.transformExpressions({
      case IR.Application.Operator.Binary(l, op, r, loc, meta) =>
        op.name match {
          case IR.Type.Ascription.name =>
            IR.Type.Ascription(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Subsumption.name =>
            IR.Type.Set
              .Subsumption(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Equality.name =>
            IR.Type.Set
              .Equality(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Concat.name =>
            IR.Type.Set
              .Concat(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Union.name =>
            IR.Type.Set
              .Union(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Intersection.name =>
            IR.Type.Set
              .Intersection(runExpression(l), runExpression(r), loc, meta)
          case IR.Type.Set.Subtraction.name =>
            IR.Type.Set
              .Subtraction(runExpression(l), runExpression(r), loc, meta)
          case _ =>
            IR.Application.Operator
              .Binary(runExpression(l), op, runExpression(r), loc, meta)
        }
    })

}

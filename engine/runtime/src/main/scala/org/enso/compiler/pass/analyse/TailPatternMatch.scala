package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.pass.IRPass

import scala.annotation.unused

case object TailPatternMatch extends IRPass {
  case object TailMatch extends IRPass.Metadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "TailMatch"

    override def duplicate(): Option[IRPass.Metadata] = Some(this)
  }

  override type Metadata = TailMatch.type
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass]   = List()
  override val invalidatedPasses: Seq[IRPass] = List()

  /** Desugars nested pattern matches in a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    @unused moduleContext: ModuleContext
  ): IR.Module = {

    ir.mapExpressions(analyseExpression)
  }

  /** Desugars nested pattern matches in an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    @unused inlineContext: InlineContext
  ): IR.Expression = {
    analyseExpression(ir)
  }

  private def analyseExpression(expr: IR.Expression): IR.Expression =
    expr.transformExpressions {
      case cse: IR.Case.Expr =>
        cse.copy(branches = cse.branches.map(analyseBranch))
    }

  private def analyseBranch(branch: IR.Case.Branch): IR.Case.Branch = {
    val expr = branch.expression match {
      case caseExpr: IR.Case.Expr =>
        caseExpr
          .updateMetadata(this -->> TailMatch)
          .copy(branches = caseExpr.branches.map(analyseBranch))
      case block @ IR.Expression.Block(exprs, ret, _, _, _, _) =>
        val newRet = ret match {
          case cs: IR.Case.Expr =>
            cs.updateMetadata(this -->> TailMatch)
              .copy(branches = cs.branches.map(analyseBranch))
          case expr => analyseExpression(expr)
        }
        val newExprs = exprs.map(analyseExpression)
        block.copy(expressions = newExprs, returnValue = newRet)
      case expr => analyseExpression(expr)
    }
    branch.copy(expression = expr)
  }
}

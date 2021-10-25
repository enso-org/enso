package org.enso.compiler.pass.optimise

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{IdentifiedLocation, Pattern}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.resolve.{DocumentationComments, IgnoredBindings}
import org.enso.syntax.text.Location

import scala.annotation.unused

/** This pass discovers and optimised away unreachable case branches.
  *
  * It removes these unreachable expressions from the IR, and attaches a
  * [[IR.Warning]] diagnostic to the case expression itself.
  *
  * Currently, a branch is considered 'unreachable' by this pass if:
  *
  * - It occurs after a catch-all branch.
  *
  * In the future, this pass should be expanded to consider patterns that are
  * entirely subsumed by previous patterns in its definition of uncreachable,
  * but this requires doing sophisticated coverage analysis, and hence should
  * happen as part of the broader refactor of nested patterns desugaring.
  *
  * This pass requires no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object UnreachableMatchBranches extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    LambdaShorthandToLambda
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    IgnoredBindings,
    NestedPatternMatch,
    TailCall
  )

  /** Runs unreachable branch optimisation on a module.
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
    ir.mapExpressions(optimizeExpression)
  }

  /** Runs unreachable branch optimisation on an expression.
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
    ir.transformExpressions { case x =>
      optimizeExpression(x)
    }
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  // === Pass Internals =======================================================

  /** Optimizes an expression by removing unreachable branches in case
    * expressions.
    *
    * @param expression the expression to optimize
    * @return `expression` with unreachable case branches removed
    */
  def optimizeExpression(expression: IR.Expression): IR.Expression = {
    expression.transformExpressions { case cse: IR.Case =>
      optimizeCase(cse)
    }
  }

  /** Optimizes a case expression by removing unreachable branches.
    *
    * Additionally, it will attach a warning about unreachable branches to the
    * case expression.
    *
    * @param cse the case expression to optimize
    * @return `cse` with unreachable branches removed
    */
  //noinspection DuplicatedCode
  def optimizeCase(cse: IR.Case): IR.Case = {
    cse match {
      case expr @ IR.Case.Expr(scrutinee, branches, _, _, _) =>
        val reachableNonCatchAllBranches = branches.takeWhile(!isCatchAll(_))
        val firstCatchAll                = branches.find(isCatchAll)
        val unreachableBranches =
          branches.dropWhile(!isCatchAll(_)).drop(1)
        val reachableBranches = firstCatchAll
          .flatMap(b => Some(reachableNonCatchAllBranches :+ b))
          .getOrElse(List())
          .toList

        if (unreachableBranches.isEmpty) {
          expr.copy(
            scrutinee = optimizeExpression(scrutinee),
            branches = branches.map(b =>
              b.copy(expression = optimizeExpression(b.expression))
            )
          )
        } else {
          val unreachableLocation =
            unreachableBranches.foldLeft(None: Option[IdentifiedLocation])(
              (loc, branch) => {
                loc match {
                  case Some(loc) =>
                    branch.location match {
                      case Some(branchLoc) =>
                        Some(
                          IdentifiedLocation(
                            Location(loc.start, branchLoc.end),
                            loc.id
                          )
                        )
                      case None => Some(loc)
                    }
                  case None => branch.location
                }
              }
            )

          val diagnostic = IR.Warning.Unreachable.Branches(unreachableLocation)

          expr
            .copy(
              scrutinee = optimizeExpression(scrutinee),
              branches = reachableBranches
                .map(b => b.copy(expression = optimizeExpression(b.expression)))
            )
            .addDiagnostic(diagnostic)
        }
      case _: IR.Case.Branch =>
        throw new CompilerError("Unexpected case branch.")
    }
  }

  /** Determines if a branch is a catch all branch.
    *
    * @param branch the branch to check
    * @return `true` if `branch` is catch-all, otherwise `false`
    */
  def isCatchAll(branch: IR.Case.Branch): Boolean = {
    branch.pattern match {
      case _: Pattern.Name          => true
      case _: Pattern.Constructor   => false
      case _: Pattern.Documentation => false
      case _: IR.Error.Pattern      => true
    }
  }
}

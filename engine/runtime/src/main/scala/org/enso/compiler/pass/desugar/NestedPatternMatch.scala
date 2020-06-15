package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Pattern
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.resolve.{DocumentationComments, IgnoredBindings}

import scala.annotation.unused

/** This pass handles the desugaring of nested pattern matches into simple
  * pattern matches (those with only one match at each level).
  *
  * It operates as a multi-stage desugaring that is best illustrated by example:
  *
  * {{{
  *   # Initial State
  *   case x of
  *       Cons (Cons a b) Nil -> a + b
  *       Cons a Nil -> a
  *       _ -> 0
  *
  *   # Desugar Nil in first branch
  *   case x of
  *       Cons (Cons a b) y -> case y of
  *           Nil -> a + b
  *           _ -> case x of
  *               Cons a Nil -> a
  *               _ -> 0
  *       Cons a Nil -> a
  *       _ -> 0
  *
  *   # Desuar `Cons a b` in the first branch
  *   case x of
  *       Cons w y -> case w of
  *           Cons a b -> case y of
  *               Nil -> a + b
  *               _ -> case x of
  *                   Cons a Nil -> a
  *                   _ -> 0
  *           _ -> case x of
  *                   Cons a Nil -> a
  *                   _ -> 0
  *       Cons a Nil -> a
  *       _ -> 0
  *
  *   # Desugar `Cons a Nil` in the second branch
  *   case x of
  *       Cons w y -> case w of
  *           Cons a b -> case y of
  *               Nil -> a + b
  *               _ -> case x of
  *                   Cons a z -> case z of
  *                       Nil -> a
  *                       _ -> case x of
  *                           _ -> 0
  *                   _ -> 0
  *           _ -> case x of
  *                   Cons a z -> case z of
  *                       Nil -> a
  *                       _ -> case x of
  *                           _ -> 0
  *                   _ -> 0
  *       Cons a z -> case z of
  *           Nil -> a
  *           _ -> case x of
  *               _ -> 0
  *       _ -> 0
  * }}}
  *
  * This pass requires no configuration.
  *
  * This pass requires the context to provide:
  *
  * - A [[FreshNameSupply]]
  */
case object NestedPatternMatch extends IRPass {
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
    TailCall
  )

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
    val freshNameSupply = moduleContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "A fresh name supply is required for nested case desugaring."
      )
    )

    ir.transformExpressions {
      case x => desugarExpression(x, freshNameSupply)
    }
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
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "A fresh name supply is required for inline nested case desugaring."
      )
    )

    ir.transformExpressions {
      case x => desugarExpression(x, freshNameSupply)
    }
  }

  // === Pass Internals =======================================================

  /** Desugars an arbitrary expression.
    *
    * @param expr the expression to desugar
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `expr`, with any nested pattern matches desugared
    */
  def desugarExpression(
    expr: IR.Expression,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    expr.transformExpressions {
      case cse: IR.Case => desugarCase(cse, freshNameSupply)
    }
  }

  /** Desugars a case expression.
    *
    * @param expr the case expression to desugar
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `expr`, with any nested patterns desugared
    */
  def desugarCase(
    expr: IR.Case,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    expr match {
      case expr @ IR.Case.Expr(scrutinee, branches, _, _, _) =>
        val scrutineeBindingName = freshNameSupply.newName()
        val scrutineeExpression  = desugarExpression(scrutinee, freshNameSupply)
        val scrutineeBinding =
          IR.Expression.Binding(scrutineeBindingName, scrutineeExpression, None)

        val caseExprScrutinee = scrutineeBindingName.duplicate(
          keepDiagnostics = false,
          keepMetadata    = false
        )

        val processedBranches = branches.zipWithIndex.map {
          case (branch, ix) =>
            val remainingBranches = branches.drop(ix + 1).toList

            desugarCaseBranch(
              branch,
              caseExprScrutinee,
              branch.location,
              remainingBranches,
              freshNameSupply
            )
        }

        val desugaredCaseExpr = expr.copy(
          scrutinee = caseExprScrutinee,
          branches  = processedBranches
        )

        IR.Expression.Block(List(scrutineeBinding), desugaredCaseExpr, None)
      case _: IR.Case.Branch =>
        throw new CompilerError(
          "Unexpected case branch during case desugaring."
        )
    }
  }

  /** Desugars a case branch.
    *
    * @param branch the branch to desugar
    * @param originalScrutinee the original scrutinee of the pattern match
    * @param topBranchLocation the location of the source branch that is being
    *                           desugared
    * @param remainingBranches all subsequent branches at the current pattern
    *                          match level
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `branch`, with any nested patterns desugared
    */
  @scala.annotation.tailrec
  def desugarCaseBranch(
    branch: IR.Case.Branch,
    originalScrutinee: IR.Expression,
    topBranchLocation: Option[IR.IdentifiedLocation],
    remainingBranches: List[IR.Case.Branch],
    freshNameSupply: FreshNameSupply
  ): IR.Case.Branch = {
    if (containsNestedPatterns(branch.pattern)) {
      branch.pattern match {
        case cons @ Pattern.Constructor(_, fields, _, _, _) =>
          // Note [Unsafe Getting the Nested Field]
          val (lastNestedPattern, nestedPosition) =
            fields.zipWithIndex.findLast { case (pat, _) => isNested(pat) }.get

          val newName  = freshNameSupply.newName()
          val newField = Pattern.Name(newName, None)
          val nestedScrutinee =
            newName.duplicate(keepDiagnostics = false, keepMetadata = false)

          val newFields =
            fields.take(nestedPosition) ++ (newField :: fields.drop(
              nestedPosition + 1
            ))

          val newPattern = cons.copy(
            fields =
              newFields.duplicate(keepDiagnostics = false, keepMetadata = false)
          )

          val newExpression = generateNestedCase(
            lastNestedPattern,
            nestedScrutinee,
            originalScrutinee,
            branch.expression,
            remainingBranches
          )

          val partDesugaredBranch = IR.Case.Branch(
            pattern = newPattern
              .duplicate(keepDiagnostics = false, keepMetadata = false),
            expression = newExpression
              .duplicate(keepDiagnostics = false, keepMetadata = false),
            None
          )

          desugarCaseBranch(
            partDesugaredBranch,
            originalScrutinee,
            topBranchLocation,
            remainingBranches,
            freshNameSupply
          )
        case _: Pattern.Name =>
          throw new CompilerError(
            "Name patterns cannot be nested. This should be unreachable."
          )
      }
    } else {
      branch.copy(
        expression = desugarExpression(branch.expression, freshNameSupply),
        location   = topBranchLocation
      )
    }
  }

  /* Note [Unsafe Getting the Nested Field]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * It is always safe to call `get` here, as the fact that we're in this `true`
   * branch for the `if` indicates that there is _at least one_ nested pattern
   * in the current pattern.
   */

  /** Generates a nested case expression of the following form.
    *
    * {{{
    *   case scrutineeName of
    *       pattern -> currentBranchExpr
    *       _ -> case topLevelScrutineeExpr of
    *           remainingBranches...
    * }}}
    *
    * @param pattern the pattern being replaced in the desugaring
    * @param nestedScrutinee the name of the variable replacing `pattern` in the
    *                      branch
    * @param topLevelScrutineeExpr the scrutinee of the original case expression
    * @param currentBranchExpr the expression executed in the current branch on
    *                          a success
    * @param remainingBranches the branches to check against on a failure
    * @return a nested case expression of the form above
    */
  def generateNestedCase(
    pattern: Pattern,
    nestedScrutinee: IR.Expression,
    topLevelScrutineeExpr: IR.Expression,
    currentBranchExpr: IR.Expression,
    remainingBranches: List[IR.Case.Branch]
  ): IR.Expression = {
    val fallbackCase = IR.Case.Expr(
      topLevelScrutineeExpr.duplicate(keepDiagnostics = false),
      remainingBranches.duplicate(keepMetadata        = false),
      None
    )

    val patternBranch =
      IR.Case.Branch(
        pattern.duplicate(keepDiagnostics        = false),
        currentBranchExpr.duplicate(keepMetadata = false),
        None
      )
    val fallbackBranch = IR.Case.Branch(
      IR.Pattern.Name(IR.Name.Blank(None), None),
      fallbackCase,
      None
    )

    IR.Case.Expr(
      nestedScrutinee.duplicate(keepDiagnostics = false, keepMetadata = false),
      List(patternBranch, fallbackBranch),
      None
    )
  }

  /** Tests if a pattern contains nested patterns.
    *
    * @param pattern the pattern to test
    * @return `true` if
    */
  def containsNestedPatterns(pattern: Pattern): Boolean = pattern match {
    case _: Pattern.Name => false
    case Pattern.Constructor(_, fields, _, _, _) =>
      fields.exists {
        case _: Pattern.Constructor => true
        case _: Pattern.Name        => false
      }
  }

  /** Checks if a given pattern is a nested pattern when called on a
    * sub-pattern.
    *
    * @param pattern the pattern to check
    * @return `true` if `pattern` is nested, otherwise `false`
    */
  def isNested(pattern: Pattern): Boolean = pattern match {
    case _: Pattern.Name        => false
    case _: Pattern.Constructor => true
  }

  /** Checks if a given pattern is a catch all branch.
    *
    * @param pattern the pattern to check
    * @return `true` if `pattern` is a catch all, otherwise `false`
    */
  def isCatchAll(pattern: Pattern): Boolean = pattern match {
    case _: Pattern.Name        => true
    case _: Pattern.Constructor => false
  }
}

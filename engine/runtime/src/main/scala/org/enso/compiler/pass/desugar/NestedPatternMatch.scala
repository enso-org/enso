package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  Expression,
  IdentifiedLocation,
  Module,
  Pattern
}
import org.enso.compiler.core.ir.expression.{errors, Case}
import org.enso.compiler.core.CompilerError
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
  *           Nil -> a + b            ## fallthrough on failed match ##
  *       Cons a Nil -> a
  *       _ -> 0
  *
  *   # Desuar `Cons a b` in the first branch
  *   case x of
  *       Cons w y -> case w of
  *           Cons a b -> case y of   ## fallthrough on failed match ##
  *               Nil -> a + b        ## fallthrough on failed match ##
  *       Cons a Nil -> a
  *       _ -> 0
  *
  *   # Desugar `Cons a Nil` in the second branch
  *   case x of
  *       Cons w y -> case w of
  *           Cons a b -> case y of   ## fallthrough on failed match ##
  *               Nil -> a + b        ## fallthrough on failed match ##
  *       Cons a z -> case z of
  *           Nil -> a                ## fallthrough on failed match ##
  *       _ -> 0
  * }}}
  *
  * Note how the desugaring discards unmatched branches for nested cases.
  * This is done on purpose to simplify the constructed IR. Rather than
  * implementing the fallthrough logic using IR, it is done in CaseNode/BranchNode
  * Truffle nodes directly.
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

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    LambdaShorthandToLambda
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
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
    ir: Module,
    @unused moduleContext: ModuleContext
  ): Module = {
    val freshNameSupply = moduleContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "A fresh name supply is required for nested case desugaring."
      )
    )

    ir.mapExpressions(desugarExpression(_, freshNameSupply))
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
    ir: Expression,
    @unused inlineContext: InlineContext
  ): Expression = {
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "A fresh name supply is required for inline nested case desugaring."
      )
    )

    ir.transformExpressions { case x =>
      desugarExpression(x, freshNameSupply)
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
    expr: Expression,
    freshNameSupply: FreshNameSupply
  ): Expression = {
    expr.transformExpressions { case cse: Case =>
      desugarCase(cse, freshNameSupply)
    }
  }

  /** Desugars a case expression.
    *
    * @param expr the case expression to desugar
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `expr`, with any nested patterns desugared
    */
  def desugarCase(
    expr: Case,
    freshNameSupply: FreshNameSupply
  ): Expression = {
    expr match {
      case expr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        val scrutineeBindingName = freshNameSupply.newName()
        val scrutineeExpression  = desugarExpression(scrutinee, freshNameSupply)
        val scrutineeBinding =
          Expression.Binding(scrutineeBindingName, scrutineeExpression, None)

        val caseExprScrutinee = scrutineeBindingName.duplicate()

        val processedBranches = branches.zipWithIndex.map { case (branch, _) =>
          desugarCaseBranch(
            branch,
            branch.location,
            freshNameSupply
          )
        }

        val desugaredCaseExpr = expr.copy(
          scrutinee = caseExprScrutinee,
          branches  = processedBranches
        )

        Expression.Block(List(scrutineeBinding), desugaredCaseExpr, None)
      case _: Case.Branch =>
        throw new CompilerError(
          "Unexpected case branch during case desugaring."
        )
    }
  }

  /** Desugars a case branch.
    *
    * @param branch the branch to desugar
    * @param topBranchLocation the location of the source branch that is being
    *                           desugared
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `branch`, with any nested patterns desugared
    */
  @scala.annotation.tailrec
  def desugarCaseBranch(
    branch: Case.Branch,
    topBranchLocation: Option[IdentifiedLocation],
    freshNameSupply: FreshNameSupply
  ): Case.Branch = {
    if (containsNestedPatterns(branch.pattern)) {
      branch.pattern match {
        case cons @ Pattern.Constructor(constrName, fields, _, _, _) =>
          // Note [Unsafe Getting the Nested Field]
          val (lastNestedPattern, nestedPosition) =
            fields.zipWithIndex.findLast { case (pat, _) => isNested(pat) }.get

          val newName  = freshNameSupply.newName(from = Some(constrName))
          val newField = Pattern.Name(newName, None)
          val nestedScrutinee =
            newName.duplicate()

          val newFields =
            fields.take(nestedPosition) ++ (newField :: fields.drop(
              nestedPosition + 1
            ))

          val newPattern = cons.copy(
            fields = newFields.duplicate()
          )

          val newExpression = generateNestedCase(
            lastNestedPattern,
            nestedScrutinee,
            branch.expression
          )

          val newPattern1 = newPattern.duplicate()
          val partDesugaredBranch = Case.Branch(
            pattern        = newPattern1,
            expression     = newExpression.duplicate(),
            terminalBranch = false,
            None
          )

          desugarCaseBranch(
            partDesugaredBranch,
            topBranchLocation,
            freshNameSupply
          )
        case _: Pattern.Literal =>
          throw new CompilerError(
            "Literal patterns cannot be nested. This should be unreachable."
          )
        case _: Pattern.Name =>
          throw new CompilerError(
            "Name patterns cannot be nested. This should be unreachable."
          )
        case _: Pattern.Type =>
          throw new CompilerError(
            "Type patterns cannot be nested. This should be unreachable."
          )
        case Pattern.Documentation(_, _, _, _) =>
          throw new CompilerError(
            "Branch documentation should be desugared at an earlier stage."
          )
        case _: errors.Pattern =>
          throw new CompilerError(
            "Error patterns cannot be nested. This should be unreachable."
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
    * @param currentBranchExpr the expression executed in the current branch on
    *                          a success
    * @return a nested case expression of the form above
    */
  def generateNestedCase(
    pattern: Pattern,
    nestedScrutinee: Expression,
    currentBranchExpr: Expression
  ): Expression = {
    val patternDuplicate = pattern.duplicate()
    val finalTest        = containsNestedPatterns(patternDuplicate)
    val patternBranch =
      Case.Branch(
        patternDuplicate,
        currentBranchExpr.duplicate(),
        terminalBranch = !finalTest,
        location       = None
      )

    Case.Expr(
      nestedScrutinee.duplicate(),
      List(patternBranch),
      isNested = true,
      location = None
    )
  }

  /** Tests if a pattern contains nested patterns.
    *
    * @param pattern the pattern to test
    * @return `true` if
    */
  def containsNestedPatterns(pattern: Pattern): Boolean =
    pattern match {
      case _: Pattern.Name => false
      case Pattern.Constructor(_, fields, _, _, _) =>
        fields.exists {
          case _: Pattern.Constructor => true
          case _: Pattern.Name        => false
          case _: Pattern.Type        => true
          case _: Pattern.Literal     => true
          case _: errors.Pattern      => false
          case _: Pattern.Documentation =>
            throw new CompilerError(
              "Branch documentation should be desugared at an earlier stage."
            )
        }
      case _: Pattern.Literal => false
      case _: Pattern.Type    => false
      case _: errors.Pattern  => false
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
    }

  /** Checks if a given pattern is a nested pattern when called on a
    * sub-pattern.
    *
    * @param pattern the pattern to check
    * @return `true` if `pattern` is nested, otherwise `false`
    */
  def isNested(pattern: Pattern): Boolean =
    pattern match {
      case _: Pattern.Name        => false
      case _: Pattern.Type        => true
      case _: Pattern.Constructor => true
      case _: Pattern.Literal     => true
      case _: errors.Pattern      => false
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
    }

  /** Checks if a given pattern is a catch all branch.
    *
    * @param pattern the pattern to check
    * @return `true` if `pattern` is a catch all, otherwise `false`
    */
  def isCatchAll(pattern: Pattern): Boolean =
    pattern match {
      case _: Pattern.Name        => true
      case _: Pattern.Constructor => false
      case _: Pattern.Literal     => false
      case _: Pattern.Type        => false
      case _: errors.Pattern      => true
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
    }
}

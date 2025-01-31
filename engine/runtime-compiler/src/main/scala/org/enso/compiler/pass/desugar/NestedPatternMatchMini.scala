package org.enso.compiler.pass.desugar

import org.enso.compiler.context.FreshNameSupply
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.core.ir.expression.{errors, Case}
import org.enso.compiler.core.ir.{Expression, IdentifiedLocation, Name, Pattern}
import org.enso.compiler.pass.MiniIRPass
import org.enso.compiler.core.Implicits.ListAsIr

import scala.annotation.unused

class NestedPatternMatchMini(
  private val freshNameSupply: FreshNameSupply
) extends MiniIRPass {

  override def prepare(
    parent: IR,
    child: Expression
  ): MiniIRPass = {
    this
  }

  override def checkPostCondition(ir: IR): Boolean = {
    ir match {
      case caseExpr: Case.Expr =>
        if (!caseExpr.scrutinee.isInstanceOf[Name.Literal]) {
          false
        } else {
          true
        }
      case _ => true
    }
  }

  override def transformExpression(
    expr: Expression
  ): Expression = {
    expr match {
      case caseExpr @ Case.Expr(scrutinee, branches, _, _, _) =>
        // Wrap the expr in block
        val scrutineeBindingName = freshNameSupply.newName()
        val scrutineeBinding =
          Expression.Binding(
            scrutineeBindingName,
            scrutinee,
            identifiedLocation = null
          )

        val caseExprScrutinee = scrutineeBindingName.duplicate()

        val processedBranches = branches.map { branch =>
          desugarCaseBranch(
            branch,
            branch.location,
            freshNameSupply
          )
        }
        val desugaredCaseExpr = caseExpr.copy(
          scrutinee = caseExprScrutinee,
          branches  = processedBranches
        )
        Expression.Block(
          expressions        = List(scrutineeBinding),
          returnValue        = desugaredCaseExpr,
          identifiedLocation = null
        )
      case _: Case.Branch =>
        expr
      case _ => expr
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
  private def desugarCaseBranch(
    branch: Case.Branch,
    topBranchLocation: Option[IdentifiedLocation],
    freshNameSupply: FreshNameSupply
  ): Case.Branch = {
    if (containsNestedPatterns(branch.pattern)) {
      branch.pattern match {
        case cons @ Pattern.Constructor(constrName, fields, _, _) =>
          // Note [Unsafe Getting the Nested Field]
          val (lastNestedPattern, nestedPosition) =
            fields.zipWithIndex.findLast { case (pat, _) => isNested(pat) }.get

          val newName  = freshNameSupply.newName(from = Some(constrName))
          val newField = Pattern.Name(newName, null)
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
            pattern            = newPattern1,
            expression         = newExpression.duplicate(),
            terminalBranch     = false,
            identifiedLocation = null
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
        case Pattern.Documentation(_, _, _) =>
          throw new CompilerError(
            "Branch documentation should be desugared at an earlier stage."
          )
        case _: errors.Pattern =>
          throw new CompilerError(
            "Error patterns cannot be nested. This should be unreachable."
          )
      }
    } else {
      branch
    }
  }

  /** Tests if a pattern contains nested patterns.
    *
    * @param pattern the pattern to test
    * @return `true` if
    */
  def containsNestedPatterns(pattern: Pattern): Boolean =
    pattern match {
      case _: Pattern.Name => false
      case Pattern.Constructor(_, fields, _, _) =>
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
  @unused
  private def generateNestedCase(
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
        terminalBranch     = !finalTest,
        identifiedLocation = null
      )

    Case.Expr(
      nestedScrutinee.duplicate(),
      List(patternBranch),
      isNested           = true,
      identifiedLocation = null
    )
  }
}

package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{Expression, Module, Name, Pattern}
import org.enso.compiler.core.ir.expression.{errors, warnings, Case}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.desugar.{GenerateMethodBodies, NestedPatternMatch}
import org.enso.compiler.pass.resolve.IgnoredBindings

import scala.annotation.unused
import scala.collection.mutable

/** This pass detects and renames shadowed pattern fields.
  *
  * This is necessary both in order to create a warning, but also to ensure that
  * alias analysis doesn't get confused.
  *
  * This pass requires no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object ShadowedPatternFields extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    GenerateMethodBodies
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    IgnoredBindings,
    NestedPatternMatch,
    TailCall
  )

  /** Lints for shadowed pattern fields.
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
    ir.mapExpressions(lintExpression)
  }

  /** Lints for shadowed pattern fields.
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
    ir.transformExpressions { case x =>
      lintExpression(x)
    }
  }

  // === Pass Internals =======================================================

  /** Lints for shadowed pattern variables on an arbitrary expression.
    *
    * @param expression the expression to lint
    * @return `expression`, with warnings for any shadowed pattern variables
    */
  def lintExpression(
    expression: Expression
  ): Expression = {
    expression.transformExpressions { case cse: Case =>
      lintCase(cse)
    }
  }

  /** Lints for shadowed pattern variables in a case expression.
    *
    * @param cse the expression to lint
    * @return `cse`, with warnings for any shadowed pattern variables
    */
  def lintCase(cse: Case): Case = {
    cse match {
      case expr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        expr.copy(
          scrutinee = lintExpression(scrutinee),
          branches  = branches.map(lintCaseBranch)
        )
      case _: Case.Branch =>
        throw new CompilerError("Unexpected case branch.")
    }
  }

  /** Lints for shadowed pattern variables in a case branch.
    *
    * @param branch the case branch to lint
    * @return `branch`, with warnings for any shadowed pattern variables
    */
  def lintCaseBranch(
    branch: Case.Branch
  ): Case.Branch = {
    branch.copy(
      pattern    = lintPattern(branch.pattern),
      expression = lintExpression(branch.expression)
    )
  }

  /** Lints a pattern for shadowed pattern variables.
    *
    * A later pattern variable shadows an earlier pattern variable with the same
    * name.
    *
    * @param pattern the pattern to lint
    * @return `pattern`, with a warning applied to any shadowed pattern
    *         variables
    */
  def lintPattern(pattern: Pattern): Pattern = {
    val seenNames: mutable.Set[String]    = mutable.Set()
    val lastSeen: mutable.Map[String, IR] = mutable.Map()

    def go(pattern: Pattern, seenNames: mutable.Set[String]): Pattern = {
      pattern match {
        case named @ Pattern.Name(name, location, _, _) =>
          if (seenNames.contains(name.name)) {
            val warning = warnings.Shadowed
              .PatternBinding(name.name, lastSeen(name.name), location)

            lastSeen(name.name) = named
            named
              .copy(
                name = Name.Blank(location = name.location)
              )
              .addDiagnostic(warning)
          } else if (!name.isInstanceOf[Name.Blank]) {
            lastSeen(name.name) = named
            seenNames += name.name
            named
          } else {
            named
          }
        case cons @ Pattern.Constructor(_, fields, _, _, _) =>
          val newFields = fields.reverse.map(go(_, seenNames)).reverse

          cons.copy(
            fields = newFields
          )
        case literal: Pattern.Literal =>
          literal
        case typed @ Pattern.Type(name, _, location, _, _) =>
          if (seenNames.contains(name.name)) {
            val warning = warnings.Shadowed
              .PatternBinding(name.name, lastSeen(name.name), location)

            lastSeen(name.name) = typed
            typed
              .copy(
                name = Name.Blank(location = name.location)
              )
              .addDiagnostic(warning)
          } else if (!name.isInstanceOf[Name.Blank]) {
            lastSeen(name.name) = typed
            seenNames += name.name
            typed
          } else {
            typed
          }
        case _: Pattern.Documentation =>
          throw new CompilerError(
            "Branch documentation should be desugared at an earlier stage."
          )
        case err: errors.Pattern => err
      }
    }

    go(pattern, seenNames)
  }
}

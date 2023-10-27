package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.{AsDiagnostics, AsMetadata}
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  Pattern,
  Warning
}
import org.enso.compiler.core.ir.expression.{errors, warnings, Case, Foreign}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.{ExpressionAnnotations, IgnoredBindings}

/** This pass performs linting for unused names, generating warnings if it finds
  * any.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object UnusedBindings extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    GenerateMethodBodies,
    IgnoredBindings,
    LambdaConsolidate,
    LambdaShorthandToLambda,
    NestedPatternMatch,
    OperatorToFunction,
    SectionsToBinOp
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List()

  /** Lints a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = if (moduleContext.compilerConfig.warningsEnabled) {
    ir.mapExpressions(
      runExpression(
        _,
        InlineContext(
          moduleContext,
          compilerConfig = moduleContext.compilerConfig
        )
      )
    )
  } else ir

  /** Lints an arbitrary expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = if (inlineContext.compilerConfig.warningsEnabled) {
    ir.transformExpressions {
      case binding: Expression.Binding => lintBinding(binding, inlineContext)
      case function: Function          => lintFunction(function, inlineContext)
      case cse: Case                   => lintCase(cse, inlineContext)
    }
  } else ir

  // === Pass Internals =======================================================

  /** Lints a binding.
    *
    * @param binding the binding to lint
    * @param context the inline context in which linting is taking place
    * @return `binding`, with any lints attached
    */
  def lintBinding(
    binding: Expression.Binding,
    context: InlineContext
  ): Expression.Binding = {
    val isIgnored = binding
      .unsafeGetMetadata(
        IgnoredBindings,
        "Binding ignore information is required for linting."
      )
      .isIgnored

    val aliasInfo = binding
      .unsafeGetMetadata(
        AliasAnalysis,
        "Aliasing information is required for linting."
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]
    val isUsed = aliasInfo.graph.linksFor(aliasInfo.id).nonEmpty

    if (!isIgnored && !isUsed) {
      binding
        .copy(expression = runExpression(binding.expression, context))
        .addDiagnostic(warnings.Unused.Binding(binding.name))
    } else {
      binding.copy(
        expression = runExpression(binding.expression, context)
      )
    }
  }

  /** Lints a function.
    *
    * @param function the function to lint
    * @param context the inline context in which linting is taking place
    * @return `function`, with any lints attached
    */
  def lintFunction(
    function: Function,
    context: InlineContext
  ): Function = {
    function match {
      case Function.Lambda(_, _: Foreign.Definition, _, _, _, _) =>
        function
      case lam @ Function.Lambda(args, body, _, _, _, _) =>
        val isBuiltin = isBuiltinMethod(body)
        val lintedArgs =
          if (isBuiltin) args
          else args.map(lintFunctionArgument(_, context))
        val body1 = runExpression(body, context)
        val lintedBody =
          if (isBuiltin)
            body match {
              case _: Literal.Text =>
                body1
              case _ =>
                body1.addDiagnostic(
                  Warning.WrongBuiltinMethod(body.location)
                )
            }
          else body1

        lam.copy(
          arguments = lintedArgs,
          body      = lintedBody
        )
      case _: Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during unused bindings linting."
        )
    }
  }

  /** Performs linting on a function argument.
    *
    * @param argument the function argument to lint
    * @param context the inline context in which linting is taking place
    * @return `argument`, with any lints attached
    */
  def lintFunctionArgument(
    argument: DefinitionArgument,
    context: InlineContext
  ): DefinitionArgument = {
    val isIgnored = argument
      .unsafeGetMetadata(
        IgnoredBindings,
        "Argument ignore information is required for linting."
      )
      .isIgnored

    val aliasInfo = argument
      .unsafeGetMetadata(
        AliasAnalysis,
        "Aliasing information missing from function argument but is " +
        "required for linting."
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]
    val isUsed = aliasInfo.graph.linksFor(aliasInfo.id).nonEmpty

    argument match {
      case s @ DefinitionArgument.Specified(
            _: Name.Self,
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        s
      case s @ DefinitionArgument.Specified(name, _, default, _, _, _, _) =>
        if (!isIgnored && !isUsed) {
          val nameToReport = name match {
            case literal: Name.Literal =>
              literal.originalName.getOrElse(literal)
            case _ => name
          }
          s.copy(
            defaultValue = default.map(runExpression(_, context))
          ).addDiagnostic(warnings.Unused.FunctionArgument(nameToReport))
        } else s
    }
  }

  /** Performs linting for unused bindings on a function argument.
    *
    * @param cse the case expression to lint
    * @param context the inline context in which linting is taking place
    * @return `cse`, with any lints attached
    */
  def lintCase(cse: Case, context: InlineContext): Case = {
    cse match {
      case expr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        expr.copy(
          scrutinee = runExpression(scrutinee, context),
          branches  = branches.map(lintCaseBranch(_, context))
        )
      case _: Case.Branch => throw new CompilerError("Unexpected case branch.")
    }
  }

  /** Performs linting for unused bindings on a case branch.
    *
    * @param branch the case branch to lint
    * @param context the inline context in which linting is taking place
    * @return `branch`, with any lints attached
    */
  def lintCaseBranch(
    branch: Case.Branch,
    context: InlineContext
  ): Case.Branch = {
    branch.copy(
      pattern    = lintPattern(branch.pattern),
      expression = runExpression(branch.expression, context)
    )
  }

  /** Performs linting for unused bindings on a pattern.
    *
    * @param pattern the pattern to lint
    * @return `pattern`, with any lints attached
    */
  def lintPattern(pattern: Pattern): Pattern = {
    pattern match {
      case n @ Pattern.Name(name, _, _, _) =>
        val isIgnored = name
          .unsafeGetMetadata(
            IgnoredBindings,
            "Free variable ignore information is required for linting."
          )
          .isIgnored

        val aliasInfo = name
          .unsafeGetMetadata(
            AliasAnalysis,
            "Aliasing information missing from pattern but is " +
            "required for linting."
          )
          .unsafeAs[AliasAnalysis.Info.Occurrence]
        val isUsed = aliasInfo.graph.linksFor(aliasInfo.id).nonEmpty

        if (!isIgnored && !isUsed) {
          n.addDiagnostic(warnings.Unused.PatternBinding(name))
        } else pattern
      case cons @ Pattern.Constructor(_, fields, _, _, _) =>
        if (!cons.isDesugared) {
          throw new CompilerError(
            "Nested patterns should not be present during linting."
          )
        }

        cons.copy(
          fields = fields.map(lintPattern)
        )
      case typed @ Pattern.Type(name, _, _, _, _) =>
        val isIgnored = name
          .unsafeGetMetadata(
            IgnoredBindings,
            "Free variable ignore information is required for linting."
          )
          .isIgnored

        val aliasInfo = name
          .unsafeGetMetadata(
            AliasAnalysis,
            "Aliasing information missing from pattern but is " +
            "required for linting."
          )
          .unsafeAs[AliasAnalysis.Info.Occurrence]
        val isUsed = aliasInfo.graph.linksFor(aliasInfo.id).nonEmpty

        if (!isIgnored && !isUsed) {
          typed.addDiagnostic(warnings.Unused.PatternBinding(name))
        } else pattern
      case literal: Pattern.Literal =>
        literal
      case err: errors.Pattern => err

      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
    }
  }

  /** Checks if the expression has a @Builtin_Method annotation
    *
    * @param expression the expression to check
    * @return 'true' if 'expression' has @Builtin_Method annotation, otherwise 'false'
    */
  private def isBuiltinMethod(expression: Expression): Boolean = {
    expression
      .getMetadata(ExpressionAnnotations)
      .exists(
        _.annotations.exists(_.name == ExpressionAnnotations.builtinMethodName)
      )
  }

}

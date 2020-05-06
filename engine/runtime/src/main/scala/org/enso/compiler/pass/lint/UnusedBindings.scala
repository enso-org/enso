package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.resolve.IgnoredBindings

import scala.annotation.unused

/** This pass performs linting for unused names, generating warnings if it finds
  * any.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  *
  * It must have the following passes run before it:
  *
  * - [[org.enso.compiler.pass.desugar.GenerateMethodBodies]]
  * - [[org.enso.compiler.pass.desugar.SectionsToBinOp]]
  * - [[org.enso.compiler.pass.desugar.OperatorToFunction]]
  * - [[org.enso.compiler.pass.desugar.LambdaShorthandToLambda]]
  * - [[IgnoredBindings]]
  * - [[org.enso.compiler.pass.optimise.LambdaConsolidate]]
  */
case object UnusedBindings extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  /** Lints a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = ir.transformExpressions {
    case x => x.mapExpressions(runExpression(_, InlineContext()))
  }

  /** Lints an arbitrary expression.
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
  ): IR.Expression = ir.transformExpressions {
    case binding: IR.Expression.Binding => lintBinding(binding, inlineContext)
    case function: IR.Function          => lintFunction(function, inlineContext)
  }

  // === Pass Internals =======================================================

  /** Lints a binding.
    *
    * @param binding the binding to lint
    * @param context the inline context in which linting is taking place
    * @return `binding`, with any lints attached
    */
  def lintBinding(
    binding: IR.Expression.Binding,
    context: InlineContext
  ): IR.Expression.Binding = {
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
      binding.copy(
        expression = runExpression(binding.expression, context)
      )
      binding.addDiagnostic(IR.Warning.Unused.Binding(binding.name))
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
    function: IR.Function,
    @unused context: InlineContext
  ): IR.Function = {
    function match {
      case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
        lam.copy(
          arguments = args.map(lintFunctionArgument(_, context)),
          body      = runExpression(body, context)
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
    argument: IR.DefinitionArgument,
    context: InlineContext
  ): IR.DefinitionArgument = {
    val isIgnored = argument
      .unsafeGetMetadata(
        IgnoredBindings,
        "Argument ignore information is required for linting."
      )
      .isIgnored

    val aliasInfo = argument
      .unsafeGetMetadata(
        AliasAnalysis,
        "Aliasing information missing but is required for linting."
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]
    val isUsed = aliasInfo.graph.linksFor(aliasInfo.id).nonEmpty

    argument match {
      case s @ IR.DefinitionArgument.Specified(name, default, _, _, _, _) =>
        if (!isIgnored && !isUsed) {
          s.copy(
              defaultValue = default.map(runExpression(_, context))
            )
            .addDiagnostic(IR.Warning.Unused.FunctionArgument(name))
        } else s
    }
  }
}

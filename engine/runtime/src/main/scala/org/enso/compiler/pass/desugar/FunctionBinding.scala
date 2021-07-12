package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.IgnoredBindings

import scala.annotation.unused

/** This pass handles the desugaring of long-form function and method
  * definitions into standard bindings using lambdas.
  *
  * This works for any definition of the form `f <args> = <body>`.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
//noinspection DuplicatedCode
case object FunctionBinding extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(ComplexType)
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    GenerateMethodBodies,
    IgnoredBindings,
    LambdaConsolidate,
    LambdaShorthandToLambda,
    NestedPatternMatch,
    OperatorToFunction,
    SectionsToBinOp,
    TailCall
  )

  /** The name of the conversion method, as a reserved name for methods. */
  val conversionMethodName: String = "from"

  /** Rusn desugaring of sugared method and function bindings on a module.
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
  ): IR.Module = ir.copy(bindings = ir.bindings.map(desugarModuleSymbol))

  /** Runs desugaring of function bindings on an arbitrary expression.
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
  ): IR.Expression = desugarExpression(ir)

  // === Pass Internals =======================================================

  /** Performs desugaring on an arbitrary Enso expression.
    *
    * @param ir the expression to desugar
    * @return `ir`, with any function definition sugar removed
    */
  def desugarExpression(ir: IR.Expression): IR.Expression = {
    ir.transformExpressions {
      case IR.Function.Binding(
            name,
            args,
            body,
            location,
            canBeTCO,
            passData,
            diagnostics
          ) =>
        if (args.isEmpty) {
          throw new CompilerError("The arguments list should not be empty.")
        }

        val lambda = args
          .map(_.mapExpressions(desugarExpression))
          .foldRight(desugarExpression(body))((arg, body) =>
            IR.Function.Lambda(List(arg), body, None)
          )
          .asInstanceOf[IR.Function.Lambda]
          .copy(canBeTCO = canBeTCO, location = location)

        IR.Expression.Binding(name, lambda, location, passData, diagnostics)
    }
  }

  /** Performs desugaring on a module definition.
    *
    * @param definition the module definition to desugar
    * @return `definition`, with any function definition sugar removed
    */
  def desugarModuleSymbol(
    definition: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition = {
    definition match {
      case a @ Definition.Atom(_, arguments, _, _, _) =>
        a.copy(arguments = arguments.map(_.mapExpressions(desugarExpression)))
      case _: Method.Explicit =>
        throw new CompilerError(
          "Explicit method definitions should not exist during function " +
          "binding desugaring."
        )
      case _: Method.Conversion =>
        throw new CompilerError(
          "Conversion method nodes should not exist during function binding " +
          "desugaring."
        )
      case meth @ Method.Binding(
            methRef,
            args,
            body,
            loc,
            passData,
            diagnostics
          ) =>
        val methodName = methRef.methodName.name

        if (methodName != conversionMethodName) {
          val newBody = args
            .map(_.mapExpressions(desugarExpression))
            .foldRight(desugarExpression(body))((arg, body) =>
              IR.Function.Lambda(List(arg), body, None)
            )

          Method.Explicit(
            methRef,
            newBody,
            loc,
            passData,
            diagnostics
          )
        } else {
          if (args.isEmpty)
            IR.Error.Conversion(meth, IR.Error.Conversion.MissingArgs)
          else if (args.head.ascribedType.isEmpty) {
            IR.Error.Conversion(
              args.head,
              IR.Error.Conversion.MissingSourceType(args.head.name.name)
            )
          } else {
            val firstArgumentType = args.head.ascribedType.get
            val nonDefaultedArg   = args.drop(1).find(_.defaultValue.isEmpty)

            if (nonDefaultedArg.isEmpty) {
              val newBody = args
                .map(_.mapExpressions(desugarExpression))
                .foldRight(desugarExpression(body))((arg, body) =>
                  IR.Function.Lambda(List(arg), body, None)
                )

              Method.Conversion(
                methRef,
                firstArgumentType,
                newBody,
                loc,
                passData,
                diagnostics
              )
            } else {
              IR.Error.Conversion(
                nonDefaultedArg.get,
                IR.Error.Conversion.NonDefaultedArgument(
                  nonDefaultedArg.get.name.name
                )
              )
            }
          }
        }
      case _: IR.Module.Scope.Definition.Type =>
        throw new CompilerError(
          "Complex type definitions should not be present during " +
          "function binding desugaring."
        )
      case _: IR.Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not be present during function binding" +
          "desugaring."
        )
      case _: IR.Name.Annotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "function binding desugaring."
        )
      case a: IR.Type.Ascription => a
      case e: IR.Error           => e
    }
  }
}

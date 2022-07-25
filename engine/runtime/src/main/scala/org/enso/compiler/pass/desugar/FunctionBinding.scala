package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.DefinitionArgument
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.core.ir.MetadataStorage.ToPair
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
import org.enso.interpreter.Constants

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

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

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
      case _: Definition.UnionType => definition
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
            val firstArg :: restArgs = args
            val firstArgumentType    = firstArg.ascribedType.get
            val firstArgumentName    = firstArg.name
            val newFirstArgument =
              if (firstArgumentName.isInstanceOf[IR.Name.Blank]) {
                val newName =
                  if (restArgs.nonEmpty)
                    IR.Name.Self(firstArgumentName.location, synthetic = true)
                  else
                    IR.Name
                      .Literal(
                        Constants.Names.THAT_ARGUMENT,
                        firstArgumentName.isMethod,
                        firstArgumentName.location
                      )
                firstArg
                  .withName(newName)
                  .updateMetadata(
                    IgnoredBindings -->> IgnoredBindings.State.Ignored
                  )
              } else {
                firstArg
              }
            val (sndArgument, remaining) = restArgs match {
              case snd :: rest =>
                val sndArgName = snd.name
                if (sndArgName.isInstanceOf[IR.Name.Blank]) {
                  val newName = IR.Name
                    .Literal(
                      Constants.Names.THAT_ARGUMENT,
                      sndArgName.isMethod,
                      sndArgName.location
                    )
                  (
                    Some(
                      snd
                        .withName(newName)
                        .updateMetadata(
                          IgnoredBindings -->> IgnoredBindings.State.Ignored
                        )
                    ),
                    rest
                  )
                } else if (snd.name.name != Constants.Names.THAT_ARGUMENT) {
                  (None, restArgs)
                } else {
                  (Some(snd), rest)
                }
              case _ =>
                (None, Nil)
            }
            def transformRemainingArgs(
              requiredArgs: List[DefinitionArgument],
              remainingArgs: List[DefinitionArgument]
            ): Either[IR.Error, IR.Module.Scope.Definition.Method] = {
              remaining
                .filter(_.name.name != Constants.Names.SELF_ARGUMENT)
                .find(_.defaultValue.isEmpty) match {
                case Some(nonDefaultedArg) =>
                  Left(
                    IR.Error.Conversion(
                      nonDefaultedArg,
                      IR.Error.Conversion.NonDefaultedArgument(
                        nonDefaultedArg.name.name
                      )
                    )
                  )
                case None =>
                  val newBody = (requiredArgs ::: remainingArgs)
                    .map(_.mapExpressions(desugarExpression))
                    .foldRight(desugarExpression(body))((arg, body) =>
                      IR.Function.Lambda(List(arg), body, None)
                    )
                  Right(
                    Method.Conversion(
                      methRef,
                      firstArgumentType,
                      newBody,
                      loc,
                      passData,
                      diagnostics
                    )
                  )
              }
            }
            val failures = sndArgument match {
              case Some(newSndArgument) =>
                if (
                  newFirstArgument.name.name == Constants.Names.SELF_ARGUMENT
                ) {
                  if (newSndArgument.name.name != Constants.Names.THAT_ARGUMENT)
                    Left(
                      IR.Error.Conversion(
                        newSndArgument,
                        IR.Error.Conversion.InvalidSourceArgumentName(
                          newSndArgument.name.name
                        )
                      )
                    )
                  else Right(())
                } else if (
                  newFirstArgument.name.name != Constants.Names.THAT_ARGUMENT
                ) {
                  Left(
                    IR.Error.Conversion(
                      newFirstArgument,
                      IR.Error.Conversion.InvalidSourceArgumentName(
                        newFirstArgument.name.name
                      )
                    )
                  )
                } else Right(())
              case None =>
                if (
                  newFirstArgument.name.name != Constants.Names.THAT_ARGUMENT
                ) {
                  Left(
                    IR.Error.Conversion(
                      newFirstArgument,
                      IR.Error.Conversion.InvalidSourceArgumentName(
                        newFirstArgument.name.name
                      )
                    )
                  )
                } else Right(())
            }
            failures
              .flatMap(_ =>
                transformRemainingArgs(
                  newFirstArgument :: sndArgument.map(List(_)).getOrElse(Nil),
                  remaining
                )
              )
              .fold(identity, identity)
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

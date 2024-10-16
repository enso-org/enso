package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ConstantsNames
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.{errors, Comment, Error}
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.IgnoredBindings

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

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(ComplexType)
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    GenerateMethodBodies,
    IgnoredBindings,
    LambdaConsolidate,
    LambdaShorthandToLambda,
    NestedPatternMatch,
    OperatorToFunction,
    SectionsToBinOp.INSTANCE,
    TailCall.INSTANCE
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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = ir.copy(bindings = ir.bindings.map(desugarModuleSymbol))

  /** Runs desugaring of function bindings on an arbitrary expression.
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
  ): Expression = desugarExpression(ir)

  // === Pass Internals =======================================================

  /** Performs desugaring on an arbitrary Enso expression.
    *
    * @param ir the expression to desugar
    * @return `ir`, with any function definition sugar removed
    */
  def desugarExpression(ir: Expression): Expression = {
    ir.transformExpressions {
      case functionBinding @ Function.Binding(
            _,
            args,
            body,
            _,
            _,
            canBeTCO,
            _
          ) =>
        if (args.isEmpty) {
          throw new CompilerError("The arguments list should not be empty.")
        }

        val lambda = args
          .map(_.mapExpressions(desugarExpression))
          .foldRight(desugarExpression(body))((arg, body) =>
            new Function.Lambda(List(arg), body, null)
          )
          .asInstanceOf[Function.Lambda]
          .copy(canBeTCO = canBeTCO, location = functionBinding.location())

        new Expression.Binding(functionBinding, lambda)
    }
  }

  /** Performs desugaring on a module definition.
    *
    * @param moduleDefinition the module definition to desugar
    * @return `definition`, with any function definition sugar removed
    */
  private def desugarModuleSymbol(
    moduleDefinition: Definition
  ): Definition = {
    moduleDefinition match {
      case _: Definition.Type =>
        moduleDefinition.mapExpressions(desugarExpression)
      case _: definition.Method.Explicit =>
        throw new CompilerError(
          "Explicit method definitions should not exist during function " +
          "binding desugaring."
        )
      case _: definition.Method.Conversion =>
        throw new CompilerError(
          "Conversion method nodes should not exist during function binding " +
          "desugaring."
        )

      // Conversion methods cannot be specified as private
      case meth @ definition.Method.Binding(
            methRef,
            _,
            isPrivate,
            _,
            _,
            _
          ) if isPrivate && methRef.methodName.name == conversionMethodName =>
        errors.Conversion(meth, errors.Conversion.DeclaredAsPrivate)

      case methodBinding @ definition.Method.Binding(
            methRef,
            args,
            isPrivate,
            body,
            _,
            _
          ) =>
        val methodName = methRef.methodName.name

        if (methodName != conversionMethodName) {
          val newBody = args
            .map(_.mapExpressions(desugarExpression))
            .foldRight(desugarExpression(body))((arg, body) =>
              new Function.Lambda(List(arg), body, null)
            )

          new definition.Method.Explicit(methodBinding, newBody)
        } else {
          if (args.isEmpty)
            errors.Conversion(methodBinding, errors.Conversion.MissingArgs)
          else if (args.head.ascribedType.isEmpty) {
            errors.Conversion(
              args.head,
              errors.Conversion.MissingSourceType(args.head.name.name)
            )
          } else {
            org.enso.common.Asserts
              .assertInJvm(!isPrivate, "Should be handled by previous match")
            val firstArg :: restArgs = args
            val firstArgumentType    = firstArg.ascribedType.get
            val firstArgumentName    = firstArg.name
            val newFirstArgument =
              if (firstArgumentName.isInstanceOf[Name.Blank]) {
                val newName =
                  if (restArgs.nonEmpty)
                    Name.Self(
                      firstArgumentName.identifiedLocation(),
                      synthetic = true
                    )
                  else
                    Name.Literal(
                      ConstantsNames.THAT_ARGUMENT,
                      firstArgumentName.isMethod,
                      firstArgumentName.identifiedLocation()
                    )
                firstArg
                  .withName(newName)
                  .updateMetadata(
                    new MetadataPair(
                      IgnoredBindings,
                      IgnoredBindings.State.Ignored
                    )
                  )
              } else {
                firstArg
              }
            val (sndArgument, remaining) = restArgs match {
              case snd :: rest =>
                val sndArgName = snd.name
                if (sndArgName.isInstanceOf[Name.Blank]) {
                  val newName = Name.Literal(
                    ConstantsNames.THAT_ARGUMENT,
                    sndArgName.isMethod,
                    sndArgName.identifiedLocation()
                  )
                  (
                    Some(
                      snd
                        .withName(newName)
                        .updateMetadata(
                          new MetadataPair(
                            IgnoredBindings,
                            IgnoredBindings.State.Ignored
                          )
                        )
                    ),
                    rest
                  )
                } else if (snd.name.name != ConstantsNames.THAT_ARGUMENT) {
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
            ): Either[Error, definition.Method] = {
              remaining
                .filter(_.name.name != ConstantsNames.SELF_ARGUMENT)
                .find(_.defaultValue.isEmpty) match {
                case Some(nonDefaultedArg) =>
                  Left(
                    errors.Conversion(
                      nonDefaultedArg,
                      errors.Conversion.NonDefaultedArgument(
                        nonDefaultedArg.name.name
                      )
                    )
                  )
                case None =>
                  val newBody = (requiredArgs ::: remainingArgs)
                    .map(_.mapExpressions(desugarExpression))
                    .foldRight(desugarExpression(body))((arg, body) =>
                      new Function.Lambda(List(arg), body, null)
                    )
                  Right(
                    new definition.Method.Conversion(
                      methodBinding,
                      firstArgumentType,
                      newBody
                    )
                  )
              }
            }
            val failures = sndArgument match {
              case Some(newSndArgument) =>
                if (
                  newFirstArgument.name.name == ConstantsNames.SELF_ARGUMENT
                ) {
                  if (newSndArgument.name.name != ConstantsNames.THAT_ARGUMENT)
                    Left(
                      errors.Conversion(
                        newSndArgument,
                        errors.Conversion.InvalidSourceArgumentName(
                          newSndArgument.name.name
                        )
                      )
                    )
                  else Right(())
                } else if (
                  newFirstArgument.name.name != ConstantsNames.THAT_ARGUMENT
                ) {
                  Left(
                    errors.Conversion(
                      newFirstArgument,
                      errors.Conversion.InvalidSourceArgumentName(
                        newFirstArgument.name.name
                      )
                    )
                  )
                } else Right(())
              case None =>
                if (
                  newFirstArgument.name.name != ConstantsNames.THAT_ARGUMENT
                ) {
                  Left(
                    errors.Conversion(
                      newFirstArgument,
                      errors.Conversion.InvalidSourceArgumentName(
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
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during " +
          "function binding desugaring."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not be present during function binding" +
          "desugaring."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "function binding desugaring."
        )
      case a: Name.GenericAnnotation => a
      case a: Type.Ascription        => a
      case e: Error                  => e
    }
  }
}

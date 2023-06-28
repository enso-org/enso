package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.core.IR.Type
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.TypeSignatures.Signature

/** This pass is responsible for analysing type signatures to determine which
  * arguments in a function definition are suspended.
  *
  * It searches for a correspondence between an argument position and the same
  * position in the associated type signature, marking the argument as suspended
  * if its type contains the _top-level_ constructor `Suspended`.
  *
  * It is a _best effort_ attempt for now, nothing more:
  *
  * - It only works on the syntactic structure of the signature.
  * - It can only deal with a correspondence between a consolidated lambda and
  *   the signature, not with extended signatures that cover returned functions
  *   and the like.
  *
  * Additionally, we currently only support looking for `Suspended` alone, as
  * supporting expressions like `Suspended a` will require the pattern contexts
  * work.
  *
  * While the `~` syntax for suspension is still supported, the signature will
  * take precedence over the `~` marking.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object SuspendedArguments extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    TypeSignatures,
    LambdaConsolidate
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall,
    UnusedBindings
  )

  /** Resolves suspended arguments in a module.
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
  ): IR.Module =
    ir.copy(
      bindings = ir.bindings.map(resolveModuleBinding)
    )

  /** Resolves suspended arguments in an arbitrary expression.
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
  ): IR.Expression = resolveExpression(ir)

  /** @inheritdoc */

  // === Pass Internals =======================================================

  /** Resolves suspended arguments for a module binding.
    *
    * It is expected that module-level type signatures _do not_ include the
    * `self` argument.
    *
    * @param binding the top-level binding to resolve suspensions in
    * @return `binding`, with any suspended arguments resolved
    */
  private def resolveModuleBinding(
    binding: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition = {
    binding match {
      case method: Method.Conversion =>
        method.body match {
          case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
            method.getMetadata(TypeSignatures) match {
              case Some(Signature(signature)) =>
                val newArgs = computeSuspensions(args.drop(1), signature)
                if (newArgs.head.suspended) {
                  IR.Error.Conversion(
                    method,
                    IR.Error.Conversion.SuspendedSourceArgument(
                      newArgs.head.name.name
                    )
                  )
                } else {
                  method.copy(body =
                    lam.copy(
                      arguments = args.head :: newArgs,
                      body      = resolveExpression(body)
                    )
                  )
                }
              case None =>
                args match {
                  case _ :: Nil =>
                    IR.Error.Conversion(
                      method,
                      IR.Error.Conversion.SuspendedSourceArgument(
                        "unknown"
                      )
                    )
                  case _ :: sourceArg :: _ if sourceArg.suspended =>
                    IR.Error.Conversion(
                      method,
                      IR.Error.Conversion.SuspendedSourceArgument(
                        sourceArg.name.name
                      )
                    )
                  case _ =>
                    method.copy(
                      body = lam.copy(body = resolveExpression(body))
                    )
                }
            }
          case _ =>
            throw new CompilerError(
              "Method bodies must be lambdas at this point."
            )
        }
      case explicit @ Method.Explicit(_, body, _, _, _) =>
        body match {
          case lam @ IR.Function.Lambda(args, lamBody, _, _, _, _) =>
            explicit.getMetadata(TypeSignatures) match {
              case Some(Signature(signature)) =>
                val newArgs = computeSuspensions(
                  args.drop(1),
                  signature
                )

                explicit.copy(body =
                  lam.copy(
                    arguments = args.head :: newArgs,
                    body      = resolveExpression(lamBody)
                  )
                )
              case None =>
                explicit.copy(
                  body = lam.copy(body = resolveExpression(lamBody))
                )
            }
          case _ =>
            throw new CompilerError(
              "Method bodies must be lambdas at this point."
            )
        }
      case _: Method.Binding  => throw new CompilerError("")
      case _: Definition.Type => binding
      case err: IR.Error      => err
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present."
        )
      case _: Type.Ascription =>
        throw new CompilerError("Type ascriptions should not be present.")
      case _: IR.Comment =>
        throw new CompilerError("Comments should not be present.")
      case _: IR.Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "suspended arguments analysis."
        )
      case ann: IR.Name.GenericAnnotation => ann
    }
  }

  /** Resolves suspended arguments in an arbitrary expression.
    *
    * @param expression the expression to perform resolution in
    * @return `expression`, with any suspended arguments resolved
    */
  private def resolveExpression(expression: IR.Expression): IR.Expression = {
    expression.transformExpressions {
      case bind @ IR.Expression.Binding(_, expr, _, _, _) =>
        val newExpr = bind.getMetadata(TypeSignatures) match {
          case Some(Signature(signature)) =>
            expr match {
              case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
                lam.copy(
                  arguments = computeSuspensions(args, signature),
                  body      = resolveExpression(body)
                )
              case _ => expr
            }
          case None => expr
        }

        bind.copy(expression = newExpr)
      case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
        lam.getMetadata(TypeSignatures) match {
          case Some(Signature(signature)) =>
            lam.copy(
              arguments = computeSuspensions(args, signature),
              body      = resolveExpression(body)
            )
          case None => lam.copy(body = resolveExpression(body))
        }

    }
  }

  /** Converts a type signature into segments.
    *
    * Segments are defined as the portions between the top-level lambdas in the
    * type signature.
    *
    * @param signature the type signature to split
    * @return the segments of `signature`
    */
  private def toSegments(signature: IR.Expression): List[IR.Expression] = {
    signature match {
      case IR.Type.Function(args, ret, _, _, _) => args :+ ret
      case _                                    => List(signature)
    }
  }

  /** Checks if a value represents a suspended argument.
    *
    * @param value the value to check
    * @return `true` if `value` represents a suspended argument, otherwise
    *         `false`
    */
  def representsSuspended(value: IR.Expression): Boolean = {
    value match {
      case IR.Name.Literal("Suspended", _, _, _, _) => true
      case _                                        => false
    }
  }

  /** Marks an argument as suspended if it corresponds to a `Suspended` portion
    * of the type signature.
    *
    * @param pair an argument and its corresponding type signature segment
    * @return the argument from `pair`, with its suspension marked appropriately
    */
  private def markSuspended(
    pair: (IR.DefinitionArgument, IR.Expression)
  ): IR.DefinitionArgument =
    pair match {
      case (arg, typ) =>
        arg match {
          case spec: IR.DefinitionArgument.Specified =>
            if (representsSuspended(typ) || spec.suspended) {
              spec.copy(suspended = true)
            } else spec.copy(suspended = false)
        }
    }

  /** Computes the suspensions for the arguments list of a function.
    *
    * @param args the function arguments
    * @param signature the signature of the function
    * @return `args`, appropriately marked as suspended or not
    */
  private def computeSuspensions(
    args: List[IR.DefinitionArgument],
    signature: IR.Expression
  ): List[IR.DefinitionArgument] = {
    val signatureSegments = toSegments(signature)

    val toComputeArgs =
      if (args.length == signatureSegments.length) {
        args.zip(signatureSegments)
      } else if (args.length > signatureSegments.length) {
        val additionalSegments = signatureSegments ::: List.fill(
          args.length - signatureSegments.length
        )(IR.Empty(None))

        args.zip(additionalSegments)
      } else {
        args.zip(
          signatureSegments
            .dropRight(signatureSegments.length - args.length)
        )
      }

    toComputeArgs.map(markSuspended)
  }
}

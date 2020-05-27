package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{ComplexType, GenerateMethodBodies, LambdaShorthandToLambda}

/** This pass translates ignored bindings (of the form `_`) into fresh names
  * internally, as well as marks all bindings as whether or not they were
  * ignored.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - A [[FreshNameSupply]].
  */
case object IgnoredBindings extends IRPass {
  override type Metadata = State
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    GenerateMethodBodies,
    LambdaShorthandToLambda
  )
  override val invalidatedPasses: Seq[IRPass] = List()

  /** Desugars ignored bindings for a module.
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
    case x =>
      x.mapExpressions(
        runExpression(
          _,
          InlineContext(freshNameSupply = moduleContext.freshNameSupply)
        )
      )
  }

  /** Desugars ignored bindings for an arbitrary expression.
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
  ): IR.Expression = {
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "Desugaring underscore arguments to lambdas requires a fresh name " +
        "supply."
      )
    )

    desugarExpression(ir, freshNameSupply)
  }

  // === Pass Internals =======================================================

  /** Desugars ignored bindings of the form `_` in an arbitrary expression.
    *
    * @param expression the expression to perform desugaring on
    * @param supply the compiler's fresh name supply
    * @return `expression`, with any ignored bidings desugared
    */
  private def desugarExpression(
    expression: IR.Expression,
    supply: FreshNameSupply
  ): IR.Expression = {
    expression.transformExpressions {
      case binding: IR.Expression.Binding => desugarBinding(binding, supply)
      case function: IR.Function          => desugarFunction(function, supply)
    }
  }

  /** Performs desugaring of ignored bindings for a binding.
    *
    * @param binding the binding to desugar
    * @param supply the compiler's supply of fresh names
    * @return `binding`, with any ignored bindings desugared
    */
  def desugarBinding(
    binding: IR.Expression.Binding,
    supply: FreshNameSupply
  ): IR.Expression.Binding = {
    if (isIgnore(binding.name)) {
      val newName = supply
        .newName()
        .copy(
          location    = binding.name.location,
          passData    = binding.name.passData,
          diagnostics = binding.name.diagnostics
        )

      binding
        .copy(
          name       = newName,
          expression = desugarExpression(binding.expression, supply)
        )
        .updateMetadata(this -->> State.Ignored)
    } else {
      binding
        .copy(
          expression = desugarExpression(binding.expression, supply)
        )
        .updateMetadata(this -->> State.NotIgnored)
    }
  }

  /** Performs desugaring of ignored function arguments.
    *
    * @param function the function to perform desugaring on
    * @param supply the compiler's fresh name supply
    * @return `function`, with any ignores desugared
    */
  def desugarFunction(
    function: IR.Function,
    supply: FreshNameSupply
  ): IR.Function = {
    function match {
      case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
        val argIsIgnore = args.map(isIgnoreArg)
        val newArgs = args.zip(argIsIgnore).map {
          case (arg, isIgnore) => genNewArg(arg, isIgnore, supply)
        }

        lam.copy(
          arguments = newArgs,
          body      = desugarExpression(body, supply)
        )
      case _: IR.Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during ignored " +
          "bindings desugaring."
        )
    }
  }

  /** Generates a new argument name for `arg` if `isIgnored` is true.
    *
    * It also handles recursing through the default values.
    *
    * @param arg the argument definition to process
    * @param isIgnored whether or not `arg` is ignored
    * @param freshNameSupply the compiler's fresh name supply
    * @return `arg`, if `isIgnored` is `false`, otherwise `arg` with a new name
    */
  def genNewArg(
    arg: IR.DefinitionArgument,
    isIgnored: Boolean,
    freshNameSupply: FreshNameSupply
  ): IR.DefinitionArgument = {
    arg match {
      case spec: IR.DefinitionArgument.Specified =>
        if (isIgnored) {
          val newName = freshNameSupply
            .newName()
            .copy(
              location    = arg.name.location,
              passData    = arg.name.passData,
              diagnostics = arg.name.diagnostics
            )

          spec
            .copy(
              name = newName,
              defaultValue =
                spec.defaultValue.map(desugarExpression(_, freshNameSupply))
            )
            .updateMetadata(this -->> State.Ignored)
        } else {
          spec
            .copy(
              defaultValue =
                spec.defaultValue.map(desugarExpression(_, freshNameSupply))
            )
            .updateMetadata(this -->> State.NotIgnored)
        }
    }
  }

  /** Checks if a given function definition argument is an ignore.
    *
    * @param ir the definition argument to check
    * @return `true` if `ir` represents an ignore, otherwise `false`
    */
  def isIgnoreArg(ir: IR.DefinitionArgument): Boolean = {
    ir match {
      case IR.DefinitionArgument.Specified(name, _, _, _, _, _) =>
        isIgnore(name)
    }
  }

  /** Checks if a given name represents an ignored argument.
    *
    * @param ir the name to check
    * @return `true` if `ir` represents an ignored argument, otherwise `false`
    */
  def isIgnore(ir: IR.Name): Boolean = {
    ir match {
      case _: IR.Name.Blank               => true
      case IR.Name.Literal(name, _, _, _) => name == "_"
      case _                              => false
    }
  }

  // === Pass Metadata ========================================================

  /** States whether or not the binding was ignored. */
  sealed trait State extends IRPass.Metadata {
    val isIgnored: Boolean
  }
  object State {

    /** States that the binding is ignored. */
    case object Ignored extends State {
      override val metadataName: String = "IgnoredBindings.State.Ignored"
      override val isIgnored: Boolean   = true
    }

    /** States that the binding is not ignored. */
    case object NotIgnored extends State {
      override val metadataName: String = "IgnoredBindings.State.NotIgnored"
      override val isIgnored: Boolean   = false
    }
  }
}

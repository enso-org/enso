package org.enso.compiler.pass.resolve

import org.enso.compiler.Compiler
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{Case, Pattern}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.desugar.{
  ComplexType,
  GenerateMethodBodies,
  LambdaShorthandToLambda,
  NestedPatternMatch
}

import scala.annotation.unused

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
    LambdaShorthandToLambda,
    NestedPatternMatch
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall
  )

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
  ): IR.Module =
    ir.mapExpressions(
      runExpression(
        _,
        InlineContext(
          moduleContext.module,
          freshNameSupply = moduleContext.freshNameSupply,
          compilerConfig  = moduleContext.compilerConfig
        )
      )
    )

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

    if (inlineContext.compilerConfig.warningsEnabled) {
      resolveExpression(ir, freshNameSupply)
    } else ir
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  private def setNotIgnored[T <: IR](ir: T): T = {
    if (ir.getMetadata(this).isEmpty) {
      ir.updateMetadata(this -->> State.NotIgnored)
    } else {
      ir
    }
  }

  // === Pass Internals =======================================================

  /** Resolves ignored bindings of the form `_` in an arbitrary expression.
    *
    * @param expression the expression to perform desugaring on
    * @param supply the compiler's fresh name supply
    * @return `expression`, with any ignored bidings desugared
    */
  private def resolveExpression(
    expression: IR.Expression,
    supply: FreshNameSupply
  ): IR.Expression = {
    expression.transformExpressions {
      case binding: IR.Expression.Binding => resolveBinding(binding, supply)
      case function: IR.Function          => resolveFunction(function, supply)
      case cse: IR.Case                   => resolveCase(cse, supply)
    }
  }

  /** Performs resolution of ignored bindings for a binding.
    *
    * @param binding the binding to desugar
    * @param supply the compiler's supply of fresh names
    * @return `binding`, with any ignored bindings desugared
    */
  def resolveBinding(
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
          expression = resolveExpression(binding.expression, supply)
        )
        .updateMetadata(this -->> State.Ignored)
    } else {
      setNotIgnored(
        binding
          .copy(
            expression = resolveExpression(binding.expression, supply)
          )
      )
    }
  }

  /** Performs resolution of ignored function arguments.
    *
    * @param function the function to perform desugaring on
    * @param supply the compiler's fresh name supply
    * @return `function`, with any ignores desugared
    */
  def resolveFunction(
    function: IR.Function,
    supply: FreshNameSupply
  ): IR.Function = {
    function match {
      case lam @ IR.Function.Lambda(args, body, _, _, _, _) =>
        val argIsIgnore = args.map(isIgnoreArg)
        val newArgs = args.zip(argIsIgnore).map { case (arg, isIgnore) =>
          genNewArg(arg, isIgnore, supply)
        }

        lam.copy(
          arguments = newArgs,
          body      = resolveExpression(body, supply)
        )
      case _: IR.Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during ignored " +
          "bindings desugaring."
        )
    }
  }

  /** Generates a new argument name for `arg` if `isIgnored` is true and updates
    * all arguments metadata with their 'ignored' status.
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
      case spec @ IR.DefinitionArgument.Specified(
            IR.Name.This(_, _, _),
            _,
            _,
            _,
            _,
            _,
            _
          ) =>
        // Note [Ignored `this` Argument]
        spec
          .copy(defaultValue =
            spec.defaultValue.map(resolveExpression(_, freshNameSupply))
          )
          .updateMetadata(this -->> State.Ignored)
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
                spec.defaultValue.map(resolveExpression(_, freshNameSupply))
            )
            .updateMetadata(this -->> State.Ignored)
        } else {
          setNotIgnored(
            spec
              .copy(
                defaultValue =
                  spec.defaultValue.map(resolveExpression(_, freshNameSupply))
              )
          )
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
      case IR.DefinitionArgument.Specified(name, _, _, _, _, _, _) =>
        isIgnore(name)
    }
  }

  /** Checks if a given name represents an ignored.
    *
    * @param ir the name to check
    * @return `true` if `ir` represents an ignore, otherwise `false`
    */
  def isIgnore(ir: IR.Name): Boolean = {
    ir match {
      case _: IR.Name.Blank                     => true
      case IR.Name.Literal(name, _, _, _, _, _) => name == "_"
      case _                                    => false
    }
  }

  /** Resolves ignored bindings in a case expression.
    *
    * @param cse the case expression to resolve ignores in
    * @param supply the compiler's fresh name supply
    * @return `cse`, with any ignored bindings resolved
    */
  def resolveCase(cse: IR.Case, supply: FreshNameSupply): IR.Case = {
    cse match {
      case expr @ Case.Expr(scrutinee, branches, _, _, _) =>
        expr.copy(
          scrutinee = resolveExpression(scrutinee, supply),
          branches  = branches.map(resolveCaseBranch(_, supply))
        )
      case _: Case.Branch =>
        throw new CompilerError(
          "Unexpected case branch while desugaring ignores for case."
        )
    }
  }

  /** Resolves ignored bindings in a case branch.
    *
    * @param branch the case branch to resolve ignores in
    * @param supply the compiler's fresh name supply
    * @return `branch`, with any ignored bindings resolved
    */
  def resolveCaseBranch(
    branch: Case.Branch,
    supply: FreshNameSupply
  ): Case.Branch = {
    branch.copy(
      pattern    = resolvePattern(branch.pattern, supply),
      expression = resolveExpression(branch.expression, supply)
    )
  }

  /** Resolves ignored bindings in a pattern.
    *
    * @param pattern the pattern to resolve ignores in
    * @param supply the compiler's fresh name supply
    * @return `pattern`, with any ignored bindings resolved
    */
  def resolvePattern(
    pattern: IR.Pattern,
    supply: FreshNameSupply
  ): IR.Pattern = {
    pattern match {
      case named @ Pattern.Name(name, _, _, _) =>
        if (isIgnore(name)) {
          val newName = supply
            .newName()
            .copy(
              location    = name.location,
              passData    = name.passData,
              diagnostics = name.diagnostics
            )
            .updateMetadata(this -->> State.Ignored)

          named.copy(
            name = newName
          )
        } else {
          named.copy(
            name = setNotIgnored(name)
          )
        }
      case cons @ Pattern.Constructor(_, fields, _, _, _) =>
        cons.copy(
          fields = fields.map(resolvePattern(_, supply))
        )
      case err: IR.Error.Pattern => err
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
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

      /** @inheritdoc */
      override def prepareForSerialization(compiler: Compiler): Ignored.type =
        this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Ignored.type] = Some(this)

      /** @inheritdoc */
      override def duplicate(): Option[IRPass.Metadata] = Some(Ignored)
    }

    /** States that the binding is not ignored. */
    case object NotIgnored extends State {
      override val metadataName: String = "IgnoredBindings.State.NotIgnored"
      override val isIgnored: Boolean   = false

      /** @inheritdoc */
      override def prepareForSerialization(
        compiler: Compiler
      ): NotIgnored.type = this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[NotIgnored.type] = Some(this)

      /** @inheritdoc */
      override def duplicate(): Option[IRPass.Metadata] = Some(NotIgnored)
    }
  }
}

/* Note [Ignored `this` Argument]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * `this` is implicitly added to all methods in the GenerateMethodBodies pass.
 * It may however not be used in the method body and this should not emit an
 * unused warning. So when processing function arguments, `this` should be
 * marked as ignored to avoid warnings from the UnusedBindings pass.
 */

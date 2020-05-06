package org.enso.compiler.pass.optimise

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.DefinitionArgument
import org.enso.compiler.core.ir.MetadataStorage
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.syntax.text.Location

/** This pass consolidates chains of lambdas into multi-argument lambdas
  * internally.
  *
  * Enso's syntax, due to its unified design, only supports single-argument
  * lambda expressions. However, internally, we want to be able to use
  * multi-argument lambda expressions for performance reasons. This pass turns
  * these chains of lambda expressions into multi-argument lambdas.
  *
  * That means that code like this:
  *
  * {{{
  *   x -> y -> z -> ...
  * }}}
  *
  * Is translated to an internal representation equivalent to
  *
  * {{{
  *   x y z -> ...
  * }}}
  *
  * Please note that this pass invalidates _all_ metdata on the transformed
  * portions of the program, and hence must be run before the deeper analysis
  * passes.
  *
  * This pass requires the context to provide:
  *
  * - A [[FreshNameSupply]].
  *
  * It must have the following passes run before it:
  *
  * - [[org.enso.compiler.pass.desugar.GenerateMethodBodies]]
  * - [[org.enso.compiler.pass.desugar.SectionsToBinOp]]
  * - [[org.enso.compiler.pass.desugar.OperatorToFunction]]
  * - [[org.enso.compiler.pass.desugar.LambdaShorthandToLambda]]
  * - [[org.enso.compiler.pass.resolve.IgnoredBindings]]
  * - [[AliasAnalysis]], which must be run _directly_ before this pass.
  */
case object LambdaConsolidate extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  /** Performs lambda consolidation on a module.
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
      runExpression(
        x,
        new InlineContext(
          freshNameSupply = moduleContext.freshNameSupply
        )
      )
  }

  /** Performs lambda consolidation on an expression.
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
        "A fresh name supply is required for lambda consolidation."
      )
    )
    ir.transformExpressions {
      case fn: IR.Function =>
        collapseFunction(fn, inlineContext, freshNameSupply)
    }
  }

  /** Collapses chained lambdas for a function definition where possible.
    *
    * @param function the function definition to optimise
    * @return the optimised version of `function`, with any directly chained
    *         lambdas collapsed
    */
  def collapseFunction(
    function: IR.Function,
    inlineContext: InlineContext,
    freshNameSupply: FreshNameSupply
  ): IR.Function = {
    function match {
      case lam @ IR.Function.Lambda(_, body, _, _, _, _) =>
        val chainedLambdas = lam :: gatherChainedLambdas(body)
        val chainedArgList =
          chainedLambdas.foldLeft(List[IR.DefinitionArgument]())(
            _ ::: _.arguments
          )
        val lastBody = chainedLambdas.last.body

        val shadowedBindingIds = getShadowedBindingIds(chainedArgList)

        val argIsShadowed = chainedArgList.map {
          case spec: IR.DefinitionArgument.Specified =>
            val aliasInfo = spec
              .unsafeGetMetadata(
                AliasAnalysis,
                "Missing aliasing information for an argument definition"
              )
              .unsafeAs[AliasAnalysis.Info.Occurrence]
            shadowedBindingIds.contains(aliasInfo.id)
        }

        val argsWithShadowed = attachShadowingWarnings(
          chainedArgList.zip(argIsShadowed)
        )
        val usageIdsForShadowed = usageIdsForShadowedArgs(argsWithShadowed)

        val newArgNames = generateNewNames(argsWithShadowed, freshNameSupply)

        val (processedArgList, newBody) =
          computeReplacedExpressions(newArgNames, lastBody, usageIdsForShadowed)

        val newLocation = chainedLambdas.head.location match {
          case Some(location) =>
            Some(
              IR.IdentifiedLocation(
                Location(
                  location.start,
                  chainedLambdas.last.location.getOrElse(location).location.end
                ),
                location.id
              )
            )
          case None => None
        }

        lam.copy(
          arguments = processedArgList,
          body      = runExpression(newBody, inlineContext),
          location  = newLocation,
          canBeTCO  = chainedLambdas.last.canBeTCO,
          passData  = MetadataStorage()
        )
    }
  }

  /** Attaches warnings to function parameters that are shadowed.
    *
    * These warnings contain the IR that is shadowing the parameter, as well as
    * the original name of the parameter.
    *
    * @param argsWithShadowed the arguments, with whether or not they are
    *                         shadowed
    * @return the list of arguments, some with attached warnings, along with
    *         whether or not they are shadowed
    */
  def attachShadowingWarnings(
    argsWithShadowed: List[(IR.DefinitionArgument, Boolean)]
  ): List[(IR.DefinitionArgument, Boolean)] = {
    val args = argsWithShadowed.map(_._1)
    val argsWithIndex =
      argsWithShadowed.zipWithIndex.map(t => (t._1._1, t._1._2, t._2))

    argsWithIndex.map {
      case (arg, isShadowed, ix) =>
        if (isShadowed) {
          val restArgs = args.drop(ix + 1)
          arg match {
            case spec @ DefinitionArgument.Specified(argName, _, _, _, _, _) =>
              val mShadower = restArgs.collectFirst {
                case s @ IR.DefinitionArgument.Specified(sName, _, _, _, _, _)
                    if sName.name == argName.name =>
                  s
              }

              val shadower: IR = mShadower.getOrElse(IR.Empty(spec.location))

              spec.diagnostics.add(
                IR.Warning.Shadowed
                  .FunctionParam(argName.name, shadower, spec.location)
              )

              (spec, isShadowed)
          }
        } else {
          (arg, isShadowed)
        }
    }
  }

  /** Generates a list of all the lambdas directly chained in the provided
    * function body.
    *
    * @param body the function body to optimise
    * @return the directly chained lambdas in `body`
    */
  def gatherChainedLambdas(body: IR.Expression): List[IR.Function.Lambda] = {
    body match {
      case l @ IR.Function.Lambda(_, body, _, _, _, _) =>
        l :: gatherChainedLambdas(body)
      case _ => List()
    }
  }

  /** Replaces all usages of an argument name in the function argument defaults
    * and the function body.
    *
    * @param body the function body
    * @param defaults the function argument defaults
    * @param argument the argument to replace occurrences with
    * @param toReplaceExpressionIds the identifiers of expressions needing
    *                               replacemebt
    * @return `body` and `defaults` with any occurrence of the old name replaced
    *        by the new name
    */
  def replaceUsages(
    body: IR.Expression,
    defaults: List[Option[IR.Expression]],
    argument: IR.DefinitionArgument,
    toReplaceExpressionIds: Set[IR.Identifier]
  ): (IR.Expression, List[Option[IR.Expression]]) = {
    (
      replaceInExpression(body, argument, toReplaceExpressionIds),
      defaults.map(
        _.map(replaceInExpression(_, argument, toReplaceExpressionIds))
      )
    )
  }

  /** Replaces usages of a name in an expression.
    *
    * As usages of a name can only be an [[IR.Name]], we can safely use the
    * expression transformation mechanism to do this.
    *
    * @param expr the expression to replace usages in
    * @param argument the argument whose usages are being replaced
    * @param toReplaceExpressionIds the identifiers of expressions that need to
    *                               be replaced
    * @return `expr`, with occurrences of the symbol for `argument` replaced
    */
  def replaceInExpression(
    expr: IR.Expression,
    argument: IR.DefinitionArgument,
    toReplaceExpressionIds: Set[IR.Identifier]
  ): IR.Expression = {
    expr.transformExpressions {
      case name: IR.Name =>
        replaceInName(name, argument, toReplaceExpressionIds)
    }
  }

  /** Replaces a name occurrence with a new name.
    *
    * @param name the IR name to replace the symbol in
    * @param argument the argument to replace the symbol in `name` with
    * @param toReplaceExpressionIds the identifiers of expressions that need
    *                               replacement
    * @return `name`, with the symbol replaced by `argument.name`
    */
  def replaceInName(
    name: IR.Name,
    argument: IR.DefinitionArgument,
    toReplaceExpressionIds: Set[IR.Identifier]
  ): IR.Name = {
    if (toReplaceExpressionIds.contains(name.getId)) {
      name match {
        case spec: IR.Name.Literal =>
          spec.copy(
            name = argument match {
              case defSpec: IR.DefinitionArgument.Specified => defSpec.name.name
            }
          )
        case ths: IR.Name.This    => ths
        case here: IR.Name.Here   => here
        case blank: IR.Name.Blank => blank
      }
    } else {
      name
    }
  }

  /** Computes the set of aliasing identifiers shadowed by the argument
    * definitions.
    *
    * @param args the consolidated list of function arguments
    * @return the set of aliasing identifiers shadowed by `args`
    */
  def getShadowedBindingIds(
    args: List[IR.DefinitionArgument]
  ): Set[AliasAnalysis.Graph.Id] = {
    args
      .map {
        case spec: IR.DefinitionArgument.Specified =>
          val aliasInfo =
            spec
              .unsafeGetMetadata(
                AliasAnalysis,
                "Missing aliasing information for an argument definition."
              )
              .unsafeAs[AliasAnalysis.Info.Occurrence]
          aliasInfo.graph
            .getOccurrence(aliasInfo.id)
            .flatMap(occ => Some(aliasInfo.graph.knownShadowedDefinitions(occ)))
            .getOrElse(Set())
      }
      .foldLeft(Set[AliasAnalysis.Graph.Occurrence]())(_ ++ _)
      .map(_.id)
  }

  /** Computes the identifiers of expression that use a shadowed argument.
    *
    * @param argsWithShadowed the argument definitions with whether or not they
    *                         are shadowed
    * @return the set of usage IR identifiers for each shadowed argument, where
    *         an empty set represents a non-shadowed argument
    */
  def usageIdsForShadowedArgs(
    argsWithShadowed: List[(IR.DefinitionArgument, Boolean)]
  ): List[Set[IR.Identifier]] = {
    argsWithShadowed.map {
      case (spec: IR.DefinitionArgument.Specified, isShadowed) =>
        val aliasInfo =
          spec
            .unsafeGetMetadata(
              AliasAnalysis,
              "Missing aliasing information for an argument definition."
            )
            .unsafeAs[AliasAnalysis.Info.Occurrence]

        // Empty set is used to indicate that it isn't shadowed
        val usageIds =
          if (isShadowed) {
            aliasInfo.graph
              .linksFor(aliasInfo.id)
              .filter(_.target == aliasInfo.id)
              .map(link => aliasInfo.graph.getOccurrence(link.source))
              .collect {
                case Some(
                    AliasAnalysis.Graph.Occurrence.Use(_, _, identifier)
                    ) =>
                  identifier
              }
          } else Set[IR.Identifier]()

        usageIds
    }
  }

  /** Generates new names for the arguments that have been shadowed.
    *
    * @param argsWithShadowed the args with whether or not they are shadowed
    * @return a set of argument names, with shadowed arguments replaced
    */
  def generateNewNames(
    argsWithShadowed: List[(IR.DefinitionArgument, Boolean)],
    freshNameSupply: FreshNameSupply
  ): List[IR.DefinitionArgument] = {
    argsWithShadowed.map {
      case (
          spec @ IR.DefinitionArgument.Specified(name, _, _, _, _, _),
          isShadowed
          ) =>
        val newName =
          if (isShadowed) {
            freshNameSupply
              .newName()
              .copy(
                location    = name.location,
                passData    = name.passData,
                diagnostics = name.diagnostics,
                id          = name.getId
              )
          } else name

        spec.copy(name = newName)
    }
  }

  /** Computes the new arguments and new function body, replacing occurrences of
    * renamed names as needed.
    *
    * @param args the arguments (already renamed)
    * @param body the function body
    * @param usageIdsForShadowed the identifiers for usages of shadowed names
    * @return `args` and `body`, with any usages of shadowed symbols replaced
    */
  def computeReplacedExpressions(
    args: List[IR.DefinitionArgument],
    body: IR.Expression,
    usageIdsForShadowed: List[Set[IR.Identifier]]
  ): (List[IR.DefinitionArgument], IR.Expression) = {
    var newBody     = body
    var newDefaults = args.map(_.defaultValue)

    val namesNeedingReplacement =
      args.zip(usageIdsForShadowed).filterNot(x => x._2.isEmpty)

    for ((arg, idents) <- namesNeedingReplacement) {
      val (updatedBody, updatedDefaults) =
        replaceUsages(newBody, newDefaults, arg, idents)

      newBody     = updatedBody
      newDefaults = updatedDefaults
    }

    val processedArgList = args.zip(newDefaults).map {
      case (spec: IR.DefinitionArgument.Specified, default) =>
        spec.copy(defaultValue = default)
    }

    (processedArgList, newBody)
  }
}

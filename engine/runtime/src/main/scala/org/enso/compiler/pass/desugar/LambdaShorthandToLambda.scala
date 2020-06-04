package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.{
  ApplicationSaturation,
  LambdaConsolidate
}
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  IgnoredBindings,
  OverloadsResolution
}

/** This pass translates `_` arguments at application sites to lambda functions.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - A [[FreshNameSupply]]
  */
case object LambdaShorthandToLambda extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    OperatorToFunction,
    SectionsToBinOp
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    ApplicationSaturation,
    DataflowAnalysis,
    DemandAnalysis,
    IgnoredBindings,
    LambdaConsolidate,
    OverloadsResolution,
    TailCall,
    UnusedBindings
  )

  /** Desugars underscore arguments to lambdas for a module.
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

  /** Desugars underscore arguments to lambdas for an arbitrary expression.
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

  /** Performs lambda shorthand desugaring on an arbitrary expression.
    *
    * @param ir the expression to desugar
    * @param freshNameSupply the compiler's fresh name supply
    * @return `ir`, with any lambda shorthand arguments desugared
    */
  def desugarExpression(
    ir: IR.Expression,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    ir.transformExpressions {
      case app: IR.Application    => desugarApplication(app, freshNameSupply)
      case caseExpr: IR.Case.Expr => desugarCaseExpr(caseExpr, freshNameSupply)
      case name: IR.Name          => desugarName(name, freshNameSupply)
    }
  }

  /** Desugars an arbitrary name occurrence, turning isolated occurrences of
    * `_` into the `id` function.
    *
    * @param name the name to desugar
    * @param supply the compiler's fresh name supply
    * @return `name`, desugared where necessary
    */
  def desugarName(name: IR.Name, supply: FreshNameSupply): IR.Expression = {
    name match {
      case blank: IR.Name.Blank =>
        val newName = supply.newName()

        IR.Function.Lambda(
          List(
            IR.DefinitionArgument.Specified(
              IR.Name.Literal(newName.name, None),
              None,
              suspended = false,
              None
            )
          ),
          newName,
          blank.location
        )
      case _ => name
    }
  }

  /** Desugars lambda shorthand arguments to an arbitrary function application.
    *
    * @param application the function application to desugar
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `application`, with any lambda shorthand arguments desugared
    */
  def desugarApplication(
    application: IR.Application,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    application match {
      case p @ IR.Application.Prefix(fn, args, _, _, _, _) =>
        // Determine which arguments are lambda shorthand
        val argIsUnderscore = determineLambdaShorthand(args)

        // Generate a new name for the arg value for each shorthand arg
        val updatedArgs =
          args
            .zip(argIsUnderscore)
            .map(updateShorthandArg(_, freshNameSupply))
            .map {
              case s @ IR.CallArgument.Specified(_, value, _, _, _, _) =>
                s.copy(value = desugarExpression(value, freshNameSupply))
            }

        // Generate a definition arg instance for each shorthand arg
        val defArgs = updatedArgs.zip(argIsUnderscore).map {
          case (arg, isShorthand) => generateDefinitionArg(arg, isShorthand)
        }
        val actualDefArgs = defArgs.collect {
          case Some(defArg) => defArg
        }

        // Determine whether or not the function itself is shorthand
        val functionIsShorthand = fn.isInstanceOf[IR.Name.Blank]
        val (updatedFn, updatedName) = if (functionIsShorthand) {
          val newFn = freshNameSupply
            .newName()
            .copy(
              location    = fn.location,
              passData    = fn.passData,
              diagnostics = fn.diagnostics
            )
          val newName = newFn.name
          (newFn, Some(newName))
        } else (fn, None)

        val processedApp = p.copy(
          function  = updatedFn,
          arguments = updatedArgs
        )

        // Wrap the app in lambdas from right to left, lambda / shorthand arg
        val appResult =
          actualDefArgs.foldRight(processedApp: IR.Expression)((arg, body) =>
            IR.Function.Lambda(List(arg), body, None)
          )

        // If the function is shorthand, do the same
        if (functionIsShorthand) {
          IR.Function.Lambda(
            List(
              IR.DefinitionArgument.Specified(
                IR.Name.Literal(updatedName.get, fn.location),
                None,
                suspended = false,
                None
              )
            ),
            appResult,
            None
          )
        } else appResult
      case f @ IR.Application.Force(tgt, _, _, _) =>
        f.copy(target = desugarExpression(tgt, freshNameSupply))
      case vector @ IR.Application.Literal.Sequence(items, _, _, _) =>
        var bindings: List[IR.Name] = List()
        val newItems = items.map {
          case blank: IR.Name.Blank =>
            val name = freshNameSupply
              .newName()
              .copy(
                location    = blank.location,
                passData    = blank.passData,
                diagnostics = blank.diagnostics
              )
            bindings ::= name
            name
          case it => desugarExpression(it, freshNameSupply)
        }
        val newVec       = vector.copy(newItems)
        val locWithoutId = newVec.location.map(_.copy(id = None))
        bindings.foldLeft(newVec: IR.Expression) { (body, bindingName) =>
          val defArg = IR.DefinitionArgument.Specified(
            bindingName,
            defaultValue = None,
            suspended    = false,
            location     = None
          )
          IR.Function.Lambda(List(defArg), body, locWithoutId)
        }
      case _: IR.Application.Operator =>
        throw new CompilerError(
          "Operators should be desugared by the point of underscore " +
          "to lambda conversion."
        )
    }
  }

  /** Determines, positionally, which of the application arguments are lambda
    * shorthand arguments.
    *
    * @param args the application arguments
    * @return a list containing `true` for a given position if the arg in that
    *         position is lambda shorthand, otherwise `false`
    */
  def determineLambdaShorthand(args: List[IR.CallArgument]): List[Boolean] = {
    args.map {
      case IR.CallArgument.Specified(_, value, _, _, _, _) =>
        value match {
          case _: IR.Name.Blank => true
          case _                => false
        }
    }
  }

  /** Generates a new name to replace a shorthand argument, as well as the
    * corresponding definition argument.
    *
    * @param argAndIsShorthand the arguments, and whether or not the argument in
    *                          the corresponding position is shorthand
    * @return the above described pair for a given position if the argument in
    *         a given position is shorthand, otherwise [[None]].
    */
  def updateShorthandArg(
    argAndIsShorthand: (IR.CallArgument, Boolean),
    freshNameSupply: FreshNameSupply
  ): IR.CallArgument = {
    val arg         = argAndIsShorthand._1
    val isShorthand = argAndIsShorthand._2

    arg match {
      case s @ IR.CallArgument.Specified(_, value, _, _, _, _) =>
        if (isShorthand) {
          val newName = freshNameSupply
            .newName()
            .copy(
              location    = value.location,
              passData    = value.passData,
              diagnostics = value.diagnostics
            )

          s.copy(value = newName)
        } else s
    }
  }

  /** Generates a corresponding definition argument to a call argument that was
    * previously lambda shorthand.
    *
    * @param arg the argument to generate a corresponding def argument to
    * @param isShorthand whether or not `arg` was shorthand
    * @return a corresponding definition argument if `arg` `isShorthand`,
    *         otherwise [[None]]
    */
  def generateDefinitionArg(
    arg: IR.CallArgument,
    isShorthand: Boolean
  ): Option[IR.DefinitionArgument] = {
    if (isShorthand) {
      arg match {
        case IR.CallArgument.Specified(_, value, _, _, _, _) =>
          // Note [Safe Casting to IR.Name.Literal]
          val defArgName =
            IR.Name.Literal(value.asInstanceOf[IR.Name.Literal].name, None)

          Some(
            IR.DefinitionArgument.Specified(
              defArgName,
              None,
              suspended = false,
              None
            )
          )
      }
    } else None
  }

  /* Note [Safe Casting to IR.Name.Literal]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * This cast is entirely safe here as, by construction in
   * `updateShorthandArg`, any arg for which `isShorthand` is true has its
   * value as an `IR.Name.Literal`.
   */

  /** Performs desugaring of lambda shorthand arguments in a case expression.
    *
    * In the case where a user writes `case _ of`, this gets converted into a
    * lambda (`x -> case x of`).
    *
    * @param caseExpr the case expression to desugar
    * @param freshNameSupply the compiler's supply of fresh names
    * @return `caseExpr`, with any lambda shorthand desugared
    */
  def desugarCaseExpr(
    caseExpr: IR.Case.Expr,
    freshNameSupply: FreshNameSupply
  ): IR.Expression = {
    val newBranches = caseExpr.branches.map(
      _.mapExpressions(expr => desugarExpression(expr, freshNameSupply))
    )

    caseExpr.scrutinee match {
      case IR.Name.Blank(loc, passData, diagnostics) =>
        val scrutineeName =
          freshNameSupply
            .newName()
            .copy(
              location    = loc,
              passData    = passData,
              diagnostics = diagnostics
            )

        val lambdaArg = IR.DefinitionArgument.Specified(
          scrutineeName.copy(id = IR.randomId),
          None,
          suspended = false,
          None
        )

        val newCaseExpr = caseExpr.copy(
          scrutinee = scrutineeName,
          branches  = newBranches
        )

        IR.Function.Lambda(
          List(lambdaArg),
          newCaseExpr,
          caseExpr.location,
          passData    = caseExpr.passData,
          diagnostics = caseExpr.diagnostics
        )
      case x =>
        caseExpr.copy(
          scrutinee = desugarExpression(x, freshNameSupply),
          branches  = newBranches
        )
    }
  }
}

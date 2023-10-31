package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.expression.{Application, Case, Operator}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.{LambdaConsolidate}
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

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    OperatorToFunction,
    SectionsToBinOp
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val new_bindings = ir.bindings.map {
      case asc: Type.Ascription => asc
      case a =>
        a.mapExpressions(
          runExpression(
            _,
            InlineContext(
              moduleContext,
              freshNameSupply = moduleContext.freshNameSupply,
              compilerConfig  = moduleContext.compilerConfig
            )
          )
        )
    }
    ir.copy(bindings = new_bindings)
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
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
    ir: Expression,
    freshNameSupply: FreshNameSupply
  ): Expression = {
    ir.transformExpressions {
      case asc: Type.Ascription => asc
      case app: Application     => desugarApplication(app, freshNameSupply)
      case caseExpr: Case.Expr  => desugarCaseExpr(caseExpr, freshNameSupply)
      case name: Name           => desugarName(name, freshNameSupply)
    }
  }

  /** Desugars an arbitrary name occurrence, turning isolated occurrences of
    * `_` into the `id` function.
    *
    * @param name the name to desugar
    * @param supply the compiler's fresh name supply
    * @return `name`, desugared where necessary
    */
  def desugarName(name: Name, supply: FreshNameSupply): Expression = {
    name match {
      case blank: Name.Blank =>
        val newName = supply.newName()

        Function.Lambda(
          List(
            DefinitionArgument.Specified(
              name = Name.Literal(
                newName.name,
                isMethod = false,
                None
              ),
              ascribedType = None,
              defaultValue = None,
              suspended    = false,
              location     = None
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
    application: Application,
    freshNameSupply: FreshNameSupply
  ): Expression = {
    application match {
      case p @ Application.Prefix(fn, args, _, _, _, _) =>
        // Determine which arguments are lambda shorthand
        val argIsUnderscore = determineLambdaShorthand(args)

        // Generate a new name for the arg value for each shorthand arg
        val updatedArgs =
          args
            .zip(argIsUnderscore)
            .map(updateShorthandArg(_, freshNameSupply))
            .map { case s @ CallArgument.Specified(_, value, _, _, _) =>
              s.copy(value = desugarExpression(value, freshNameSupply))
            }

        // Generate a definition arg instance for each shorthand arg
        val defArgs = updatedArgs.zip(argIsUnderscore).map {
          case (arg, isShorthand) => generateDefinitionArg(arg, isShorthand)
        }
        val actualDefArgs = defArgs.collect { case Some(defArg) =>
          defArg
        }

        // Determine whether or not the function itself is shorthand
        val functionIsShorthand = fn.isInstanceOf[Name.Blank]
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
        } else {
          val newFn = desugarExpression(fn, freshNameSupply)
          (newFn, None)
        }

        val processedApp = p.copy(
          function  = updatedFn,
          arguments = updatedArgs
        )

        // Wrap the app in lambdas from right to left, 1 lambda per shorthand
        // arg
        val appResult =
          actualDefArgs.foldRight(processedApp: Expression)((arg, body) =>
            Function.Lambda(List(arg), body, None)
          )

        // If the function is shorthand, do the same
        val resultExpr = if (functionIsShorthand) {
          Function.Lambda(
            List(
              DefinitionArgument.Specified(
                Name
                  .Literal(
                    updatedName.get,
                    isMethod = false,
                    fn.location
                  ),
                None,
                None,
                suspended = false,
                None
              )
            ),
            appResult,
            None
          )
        } else appResult

        resultExpr match {
          case lam: Function.Lambda => lam.copy(location = p.location)
          case result               => result
        }
      case f @ Application.Force(tgt, _, _, _) =>
        f.copy(target = desugarExpression(tgt, freshNameSupply))
      case vector @ Application.Sequence(items, _, _, _) =>
        var bindings: List[Name] = List()
        val newItems = items.map {
          case blank: Name.Blank =>
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
        bindings.foldLeft(newVec: Expression) { (body, bindingName) =>
          val defArg = DefinitionArgument.Specified(
            bindingName,
            ascribedType = None,
            defaultValue = None,
            suspended    = false,
            location     = None
          )
          Function.Lambda(List(defArg), body, locWithoutId)
        }
      case tSet @ Application.Typeset(expr, _, _, _) =>
        tSet.copy(expression = expr.map(desugarExpression(_, freshNameSupply)))
      case _: Operator =>
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
  def determineLambdaShorthand(args: List[CallArgument]): List[Boolean] = {
    args.map { case CallArgument.Specified(_, value, _, _, _) =>
      value match {
        case _: Name.Blank => true
        case _             => false
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
    argAndIsShorthand: (CallArgument, Boolean),
    freshNameSupply: FreshNameSupply
  ): CallArgument = {
    val arg         = argAndIsShorthand._1
    val isShorthand = argAndIsShorthand._2

    arg match {
      case s @ CallArgument.Specified(_, value, _, _, _) =>
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
    arg: CallArgument,
    isShorthand: Boolean
  ): Option[DefinitionArgument] = {
    if (isShorthand) {
      arg match {
        case CallArgument.Specified(_, value, _, passData, diagnostics) =>
          // Note [Safe Casting to Name.Literal]
          val defArgName =
            Name.Literal(
              value.asInstanceOf[Name.Literal].name,
              isMethod = false,
              None
            )

          Some(
            DefinitionArgument.Specified(
              defArgName,
              None,
              None,
              suspended = false,
              None,
              passData.duplicate,
              diagnostics.copy
            )
          )
      }
    } else None
  }

  /* Note [Safe Casting to Name.Literal]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * This cast is entirely safe here as, by construction in
   * `updateShorthandArg`, any arg for which `isShorthand` is true has its
   * value as an `Name.Literal`.
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
    caseExpr: Case.Expr,
    freshNameSupply: FreshNameSupply
  ): Expression = {
    val newBranches = caseExpr.branches.map(
      _.mapExpressions(expr => desugarExpression(expr, freshNameSupply))
    )

    caseExpr.scrutinee match {
      case Name.Blank(loc, passData, diagnostics) =>
        val scrutineeName =
          freshNameSupply
            .newName()
            .copy(
              location    = loc,
              passData    = passData,
              diagnostics = diagnostics
            )

        val lambdaArg = DefinitionArgument.Specified(
          scrutineeName.copy(id = IR.randomId),
          None,
          None,
          suspended = false,
          None
        )

        val newCaseExpr = caseExpr.copy(
          scrutinee = scrutineeName,
          branches  = newBranches
        )

        Function.Lambda(
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

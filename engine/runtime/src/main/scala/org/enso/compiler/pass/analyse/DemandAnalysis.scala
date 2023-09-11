package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Empty,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.expression.{
  Application,
  Case,
  Comment,
  Error,
  Foreign,
  Operator
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.OverloadsResolution

/** This pass implements demand analysis for Enso.
  *
  * Demand analysis is the process of determining _when_ a suspended term needs
  * to be forced (where the suspended value is _demanded_).
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  *
  * Additionally, all members of [[org.enso.compiler.core.ir.IRKind.Primitive]] must have been removed
  * from the IR by the time it runs.
  */
case object DemandAnalysis extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    AliasAnalysis,
    LambdaConsolidate,
    OverloadsResolution
  )

  override val invalidatedPasses: Seq[IRPass] = List(AliasAnalysis)

  /** Executes the demand analysis process on an Enso module.
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
    ir.copy(bindings =
      ir.bindings.map(t =>
        t.mapExpressions(
          runExpression(
            _,
            InlineContext(
              moduleContext,
              compilerConfig = moduleContext.compilerConfig
            )
          )
        )
      )
    )
  }

  /** Executes the demand analysis process on an Enso expression.
    *
    * @param expression the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, transformed to correctly force terms
    */
  override def runExpression(
    expression: Expression,
    inlineContext: InlineContext
  ): Expression =
    analyseExpression(
      expression,
      isInsideCallArgument = false
    )

  /** Performs demand analysis on an arbitrary program expression.
    *
    * @param expression the expression to perform demand analysis on
    * @param isInsideCallArgument whether the current expression occurs _inside_
    *                             a call argument (note that this should not be
    *                             set for the call argument itself)
    * @return `expression`, transformed by the demand analysis process
    */
  def analyseExpression(
    expression: Expression,
    isInsideCallArgument: Boolean
  ): Expression = {
    expression match {
      case empty: Empty => empty
      case fn: Function => analyseFunction(fn)
      case name: Name   => analyseName(name, isInsideCallArgument)
      case app: Application =>
        analyseApplication(app, isInsideCallArgument)
      case typ: Type =>
        analyseType(typ, isInsideCallArgument)
      case cse: Case =>
        analyseCase(cse, isInsideCallArgument)
      case block @ Expression.Block(expressions, retVal, _, _, _, _) =>
        block.copy(
          expressions = expressions.map(x =>
            analyseExpression(x, isInsideCallArgument = false)
          ),
          returnValue = analyseExpression(retVal, isInsideCallArgument = false)
        )
      case binding @ Expression.Binding(_, expression, _, _, _) =>
        binding.copy(expression =
          analyseExpression(
            expression,
            isInsideCallArgument = false
          )
        )
      case lit: Literal     => lit
      case err: Error       => err
      case foreign: Foreign => foreign
      case comment: Comment =>
        comment.mapExpressions(x =>
          analyseExpression(
            x,
            isInsideCallArgument
          )
        )
    }
  }

  /** Performs demand analysis for a function.
    *
    * @param function the function to perform demand analysis on
    * @return `function`, transformed by the demand analysis process
    */
  def analyseFunction(
    function: Function
  ): Function =
    function match {
      case lam @ Function.Lambda(args, body, _, _, _, _) =>
        lam.copy(
          arguments = args.map(analyseDefinitionArgument),
          body = analyseExpression(
            body,
            isInsideCallArgument = false
          )
        )
      case _: Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during demand analysis."
        )
    }

  /** Performs demand analysis for a name.
    *
    * If the name refers to a term that is suspended, this name is forced unless
    * it is being passed to a function. If the name is being passed to a function
    * it is passed raw.
    *
    * @param name the name to perform demand analysis on.
    * @param isInsideCallArgument whether or not the name occurs inside a call
    *                             call argument
    * @return `name`, transformed by the demand analysis process
    */
  def analyseName(
    name: Name,
    isInsideCallArgument: Boolean
  ): Expression = {
    if (isInsideCallArgument) {
      name
    } else {
      name match {
        case lit: Name.Literal if isDefined(lit) =>
          val forceLocation   = name.location
          val newNameLocation = name.location.map(l => l.copy(id = None))
          val newName         = lit.copy(location = newNameLocation)
          Application.Force(newName, forceLocation)
        case _ => name
      }
    }
  }

  private def isDefined(name: Name): Boolean = {
    val aliasInfo = name
      .unsafeGetMetadata(
        AliasAnalysis,
        "Missing alias occurrence information for a name usage"
      )
      .unsafeAs[AliasAnalysis.Info.Occurrence]

    aliasInfo.graph.defLinkFor(aliasInfo.id).isDefined
  }

  /** Performs demand analysis on an application.
    *
    * @param application the function application to perform demand analysis on
    * @param isInsideCallArgument whether or not the application is occurring
    *                             inside a call argument
    * @return `application`, transformed by the demand analysis process
    */
  def analyseApplication(
    application: Application,
    isInsideCallArgument: Boolean
  ): Application =
    application match {
      case pref @ Application.Prefix(fn, args, _, _, _, _) =>
        val newFun = fn match {
          case n: Name => n
          case e       => analyseExpression(e, isInsideCallArgument = false)
        }
        pref.copy(
          function  = newFun,
          arguments = args.map(analyseCallArgument)
        )
      case force @ Application.Force(target, _, _, _) =>
        force.copy(target =
          analyseExpression(
            target,
            isInsideCallArgument
          )
        )
      case vec @ Application.Sequence(items, _, _, _) =>
        vec.copy(items =
          items.map(
            analyseExpression(
              _,
              isInsideCallArgument = false
            )
          )
        )
      case tSet @ Application.Typeset(expr, _, _, _) =>
        tSet.copy(
          expression =
            expr.map(analyseExpression(_, isInsideCallArgument = false))
        )
      case _: Operator =>
        throw new CompilerError(
          "Operators should not be present during demand analysis."
        )
    }

  /** Performs demand analysis on a function call argument.
    *
    * In keeping with the requirement by the runtime to pass all function
    * arguments as thunks, we mark the argument as needing suspension based on
    * whether it already is a thunk or not.
    *
    * @param arg the argument to perform demand analysis on
    * @return `arg`, transformed by the demand analysis process
    */
  def analyseCallArgument(arg: CallArgument): CallArgument = {
    arg match {
      case spec @ CallArgument.Specified(_, expr, _, _, _) =>
        spec.copy(
          value = analyseExpression(
            expr,
            isInsideCallArgument = true
          )
        )
    }
  }

  /** Performs demand analysis on a function definition argument.
    *
    * @param arg the argument to perform demand analysis on
    * @return `arg`, transformed by the demand analysis process
    */
  def analyseDefinitionArgument(
    arg: DefinitionArgument
  ): DefinitionArgument = {
    arg match {
      case spec @ DefinitionArgument.Specified(_, _, default, _, _, _, _) =>
        spec.copy(
          defaultValue = default.map(x =>
            analyseExpression(
              x,
              isInsideCallArgument = false
            )
          )
        )
    }
  }

  /** Performs demand analysis on a typing expression.
    *
    * @param typ the expression to perform demand analysis on
    * @param isInsideCallArgument whether the typing expression occurs inside a
    *                             function call argument
    * @return `typ`, transformed by the demand analysis process
    */
  def analyseType(
    typ: Type,
    isInsideCallArgument: Boolean
  ): Type =
    typ.mapExpressions(x => analyseExpression(x, isInsideCallArgument))

  /** Performs demand analysis on a case expression.
    *
    * @param cse the case expression to perform demand analysis on
    * @param isInsideCallArgument whether the case expression occurs inside a
    *                             function call argument
    * @return `cse`, transformed by the demand analysis process
    */
  def analyseCase(
    cse: Case,
    isInsideCallArgument: Boolean
  ): Case =
    cse match {
      case expr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        expr.copy(
          scrutinee = analyseExpression(
            scrutinee,
            isInsideCallArgument
          ),
          branches = branches.map(b => analyseCaseBranch(b))
        )
      case _ => throw new CompilerError("Unexpected case construct.")
    }

  /** Performs demand analysis on a case branch.
    *
    * @param branch the case branch to perform demand analysis on
    * @return `branch`, transformed by the demand analysis process
    */
  def analyseCaseBranch(branch: Case.Branch): Case.Branch = {
    branch.copy(
      expression = analyseExpression(
        branch.expression,
        isInsideCallArgument = false
      )
    )
  }
}

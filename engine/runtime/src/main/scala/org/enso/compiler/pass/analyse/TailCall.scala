package org.enso.compiler.pass.analyse

import org.enso.compiler.InlineContext
import org.enso.compiler.core.IR
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass

/** This pass performs tail call analysis on the Enso IR.
  *
  * It is responsible for marking every single expression with whether it is in
  * tail position or not. This allows the code generator to correctly create the
  * Truffle nodes.
  */
case object TailCall extends IRPass {

  /** The annotation metadata type associated with IR nodes by this pass. */
  override type Metadata = TailPosition

  /** Analyses tail call state for expressions in a module.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module = {
    ir.copy(bindings = ir.bindings.map(analyseModuleBinding))
  }

  /** Analyses tail call state for an arbitrary expression.
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
  ): IR.Expression =
    analyseExpression(
      ir,
      inlineContext.isInTailPosition.getOrElse(
        throw new CompilerError(
          "Information about the tail position for an inline expression " +
          "must be known by the point of tail call analysis."
        )
      )
    )

  /** Performs tail call analysis on a top-level definition in a module.
    *
    * @param definition the top-level definition to analyse
    * @return `definition`, annotated with tail call information
    */
  def analyseModuleBinding(
    definition: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition = {
    definition match {
      case method @ IR.Module.Scope.Definition.Method(_, _, body, _, _) =>
        method
          .copy(
            body = analyseExpression(body, isInTailPosition = true)
          )
          .addMetadata(TailPosition.Tail)
      case atom @ IR.Module.Scope.Definition.Atom(_, args, _, _) =>
        atom
          .copy(
            arguments = args.map(analyseDefArgument)
          )
          .addMetadata(TailPosition.Tail)
    }
  }

  /** Performs tail call analysis on an arbitrary expression.
    *
    * @param expression the expression to analyse
    * @param isInTailPosition whether or not the expression is occurring in tail
    *                         position
    * @return `expression`, annotated with tail position metadata
    */
  def analyseExpression(
    expression: IR.Expression,
    isInTailPosition: Boolean
  ): IR.Expression = {
    expression match {
      case function: IR.Function => analyseFunction(function, isInTailPosition)
      case caseExpr: IR.Case     => analyseCase(caseExpr, isInTailPosition)
      case typ: IR.Type          => analyseType(typ, isInTailPosition)
      case app: IR.Application   => analyseApplication(app, isInTailPosition)
      case name: IR.Name         => analyseName(name, isInTailPosition)
      case foreign: IR.Foreign   => foreign.addMetadata(TailPosition.NotTail)
      case literal: IR.Literal   => analyseLiteral(literal, isInTailPosition)
      case comment: IR.Comment   => analyseComment(comment, isInTailPosition)
      case block @ IR.Expression.Block(expressions, returnValue, _, _, _) =>
        block
          .copy(
            expressions =
              expressions.map(analyseExpression(_, isInTailPosition = false)),
            returnValue = analyseExpression(returnValue, isInTailPosition)
          )
          .addMetadata(TailPosition.fromBool(isInTailPosition))
      case binding @ IR.Expression.Binding(_, expression, _, _) =>
        binding
          .copy(
            expression = analyseExpression(expression, isInTailPosition)
          )
          .addMetadata(TailPosition.fromBool(isInTailPosition))
      case err: IR.Error => err
    }
  }

  /** Performs tail call analysis on an occurrence of a name.
    *
    * @param name the name to analyse
    * @param isInTailPosition whether the name occurs in tail position or not
    * @return `name`, annotated with tail position metadata
    */
  def analyseName(name: IR.Name, isInTailPosition: Boolean): IR.Name = {
    name.addMetadata(TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a comment occurrence.
    *
    * @param comment the comment to analyse
    * @param isInTailPosition whether the comment occurs in tail position or not
    * @return `comment`, annotated with tail position metadata
    */
  def analyseComment(
    comment: IR.Comment,
    isInTailPosition: Boolean
  ): IR.Comment = {
    comment match {
      case doc @ IR.Comment.Documentation(expr, _, _, _) =>
        doc
          .copy(commented = analyseExpression(expr, isInTailPosition))
          .addMetadata(TailPosition.fromBool(isInTailPosition))
    }
  }

  /** Performs tail call analysis on a literal.
    *
    * @param literal the literal to analyse
    * @param isInTailPosition whether or not the literal occurs in tail position
    *                         or not
    * @return `literal`, annotated with tail position metdata
    */
  def analyseLiteral(
    literal: IR.Literal,
    isInTailPosition: Boolean
  ): IR.Literal = {
    literal.addMetadata(TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on an application.
    *
    * @param application the application to analyse
    * @param isInTailPosition whether or not the application is occurring in
    *                         tail position
    * @return `application`, annotated with tail position metadata
    */
  def analyseApplication(
    application: IR.Application,
    isInTailPosition: Boolean
  ): IR.Application = {
    application match {
      case app @ IR.Application.Prefix(fn, args, _, _, _) =>
        app
          .copy(
            function  = analyseExpression(fn, isInTailPosition = false),
            arguments = args.map(analyseCallArg)
          )
          .addMetadata(TailPosition.fromBool(isInTailPosition))
      case force @ IR.Application.Force(target, _, _) =>
        force
          .copy(
            target = analyseExpression(target, isInTailPosition)
          )
          .addMetadata(TailPosition.fromBool(isInTailPosition))
      case _: IR.Application.Operator =>
        throw new CompilerError("Unexpected binary operator.")
    }
  }

  /** Performs tail call analysis on a call site argument.
    *
    * @param argument the argument to analyse
    * @return `argument`, annotated with tail position metadata
    */
  def analyseCallArg(argument: IR.CallArgument): IR.CallArgument = {
    argument match {
      case arg @ IR.CallArgument.Specified(_, expr, _, _, _) =>
        arg
          .copy(
            // Note [Call Argument Tail Position]
            value = analyseExpression(expr, isInTailPosition = true)
          )
          .addMetadata(TailPosition.Tail)
    }
  }

  /* Note [Call Argument Tail Position]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * In order to efficiently deal with Enso's ability to suspend function
   * arguments, we behave as if all arguments to a function are passed as
   * thunks. This means that the _function_ becomes responsible for deciding
   * when to evaluate its arguments.
   *
   * Conceptually, this results in a desugaring as follows:
   *
   * ```
   * foo a b c
   * ```
   *
   * Becomes:
   *
   * ```
   * foo ({} -> a) ({} -> b) ({} -> c)
   * ```
   *
   * Quite obviously, the arguments `a`, `b` and `c` are in tail position in
   * these closures, and hence should be marked as tail.
   */

  /** Performs tail call analysis on an expression involving type operators.
    *
    * @param value the type operator expression
    * @param isInTailPosition whether or not the type operator occurs in a tail
    *                         call position
    * @return `value`, annotated with tail position metadata
    */
  def analyseType(value: IR.Type, isInTailPosition: Boolean): IR.Type = {
    value
      .mapExpressions(analyseExpression(_, isInTailPosition = false))
      .addMetadata(TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a case expression.
    *
    * @param caseExpr the case expression to analyse
    * @param isInTailPosition whether or not the case expression occurs in a tail
    *                         call position
    * @return `caseExpr`, annotated with tail position metadata
    */
  def analyseCase(caseExpr: IR.Case, isInTailPosition: Boolean): IR.Case = {
    caseExpr match {
      case caseExpr @ IR.Case.Expr(scrutinee, branches, fallback, _, _) =>
        caseExpr
          .copy(
            scrutinee = analyseExpression(scrutinee, isInTailPosition = false),
            // Note [Analysing Branches in Case Expressions]
            branches = branches.map(analyseCaseBranch(_, isInTailPosition)),
            fallback = fallback.map(analyseExpression(_, isInTailPosition))
          )
          .addMetadata(TailPosition.fromBool(isInTailPosition))
      case _: IR.Case.Branch =>
        throw new CompilerError("Unexpected case branch.")
    }
  }

  /* Note [Analysing Branches in Case Expressions]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * When performing tail call analysis on a case expression it is very
   * important to recognise that the branches of a case expression should all
   * have the same tail call state. The branches should only be marked as being
   * in tail position when the case expression _itself_ is in tail position.
   *
   * As only one branch is ever executed, it is hence safe to mark _all_
   * branches as being in tail position if the case expression is.
   */

  /** Performs tail call analysis on a case branch.
    *
    * @param branch the branch to analyse
    * @param isInTailPosition whether or not the branch occurs in a tail call
    *                         position
    * @return `branch`, annotated with tail position metadata
    */
  def analyseCaseBranch(
    branch: IR.Case.Branch,
    isInTailPosition: Boolean
  ): IR.Case.Branch = {
    branch
      .copy(
        pattern = analyseExpression(branch.pattern, isInTailPosition = false),
        expression = analyseExpression(
          branch.expression,
          isInTailPosition
        )
      )
      .addMetadata(TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a function definition.
    *
    * @param function the function to analyse
    * @param isInTailPosition whether or not the function definition occurs in a
    *                         tail position
    * @return `function`, annotated with tail position metadata
    */
  def analyseFunction(
    function: IR.Function,
    isInTailPosition: Boolean
  ): IR.Function = {
    val canBeTCO   = function.canBeTCO
    val markAsTail = (!canBeTCO && isInTailPosition) || canBeTCO

    val resultFunction = function match {
      case lambda @ IR.Function.Lambda(args, body, _, _, _) =>
        lambda.copy(
          arguments = args.map(analyseDefArgument),
          body      = analyseExpression(body, isInTailPosition = markAsTail)
        )
    }

    resultFunction.addMetadata(
      TailPosition.fromBool(isInTailPosition)
    )
  }

  /** Performs tail call analysis on a function definition argument.
    *
    * @param arg the argument definition to analyse
    * @return `arg`, annotated with tail position metadata
    */
  def analyseDefArgument(arg: IR.DefinitionArgument): IR.DefinitionArgument = {
    arg match {
      case arg @ IR.DefinitionArgument.Specified(_, default, _, _, _) =>
        arg
          .copy(
            defaultValue = default.map(
              x =>
                analyseExpression(x, isInTailPosition = false)
                  .addMetadata(TailPosition.NotTail)
            )
          )
          .addMetadata(TailPosition.NotTail)
      case err: IR.Error.Redefined.Argument => err
    }
  }

  /** Expresses the tail call state of an IR Node. */
  sealed trait TailPosition extends IR.Metadata {

    /** A boolean representation of the expression's tail state. */
    def isTail: Boolean
  }
  object TailPosition {

    /** The expression is in a tail position and can be tail call optimised. */
    final case object Tail extends TailPosition {
      override def isTail: Boolean = true
    }

    /** The expression is not in a tail position and cannot be tail call
      * optimised.
      */
    final case object NotTail extends TailPosition {
      override def isTail: Boolean = false
    }

    /** Implicitly converts a boolean to a [[TailPosition]] value.
      *
      * @param isTail the boolean
      * @return the tail position value corresponding to `bool`
      */
    implicit def fromBool(isTail: Boolean): TailPosition = {
      if (isTail) TailPosition.Tail else TailPosition.NotTail
    }

    /** Implicitly converts the tail position data into a boolean.
      *
      * @param tailPosition the tail position value
      * @return the boolean value corresponding to `tailPosition`
      */
    implicit def toBool(tailPosition: TailPosition): Boolean = {
      tailPosition.isTail
    }
  }
}

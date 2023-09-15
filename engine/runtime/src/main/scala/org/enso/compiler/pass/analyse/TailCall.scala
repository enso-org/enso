package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.{
  errors,
  Application,
  Case,
  Comment,
  Error,
  Foreign,
  Operator
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Diagnostic,
  Empty,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  Pattern,
  Type,
  Warning
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.resolve.{ExpressionAnnotations, GlobalNames}

/** This pass performs tail call analysis on the Enso IR.
  *
  * It is responsible for marking every single expression with whether it is in
  * tail position or not. This allows the code generator to correctly create the
  * Truffle nodes.
  *
  * This pass requires the context to provide:
  *
  * - The tail position of its expression, where relevant.
  */
case object TailCall extends IRPass {

  /** The annotation metadata type associated with IR nodes by this pass. */
  override type Metadata = TailPosition

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp,
    OperatorToFunction,
    LambdaShorthandToLambda,
    GlobalNames
  )

  override val invalidatedPasses: Seq[IRPass] = List()

  /** Analyses tail call state for expressions in a module.
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression =
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
  private def analyseModuleBinding(
    moduleDefinition: Definition
  ): Definition = {
    moduleDefinition match {
      case method: definition.Method.Conversion =>
        method
          .copy(
            body = analyseExpression(method.body, isInTailPosition = true)
          )
          .updateMetadata(this -->> TailPosition.Tail)
      case method @ definition.Method
            .Explicit(_, body, _, _, _) =>
        method
          .copy(
            body = analyseExpression(body, isInTailPosition = true)
          )
          .updateMetadata(this -->> TailPosition.Tail)
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during tail call " +
          "analysis."
        )
      case _: Definition.Type =>
        moduleDefinition.updateMetadata(this -->> TailPosition.Tail)
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during " +
          "tail call analysis."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not exist as an entity during tail call analysis."
        )
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type signatures should not exist at the top level during " +
          "tail call analysis."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "tail call analysis."
        )
      case ann: Name.GenericAnnotation =>
        ann
          .copy(expression =
            analyseExpression(ann.expression, isInTailPosition = true)
          )
          .updateMetadata(this -->> TailPosition.Tail)
      case err: Error => err
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
    expression: Expression,
    isInTailPosition: Boolean
  ): Expression = {
    val expressionWithWarning =
      if (isTailAnnotated(expression) && !isInTailPosition)
        expression.addDiagnostic(Warning.WrongTco(expression.location))
      else expression
    expressionWithWarning match {
      case empty: Empty =>
        empty.updateMetadata(this -->> TailPosition.NotTail)
      case function: Function =>
        analyseFunction(function, isInTailPosition)
      case caseExpr: Case   => analyseCase(caseExpr, isInTailPosition)
      case typ: Type        => analyseType(typ, isInTailPosition)
      case app: Application => analyseApplication(app, isInTailPosition)
      case name: Name       => analyseName(name, isInTailPosition)
      case foreign: Foreign =>
        foreign.updateMetadata(this -->> TailPosition.NotTail)
      case literal: Literal => analyseLiteral(literal, isInTailPosition)
      case _: Comment =>
        throw new CompilerError(
          "Comments should not be present during tail call analysis."
        )
      case block @ Expression.Block(
            expressions,
            returnValue,
            _,
            _,
            _,
            _
          ) =>
        block
          .copy(
            expressions = expressions.map(
              analyseExpression(_, isInTailPosition = false)
            ),
            returnValue = analyseExpression(returnValue, isInTailPosition)
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case binding @ Expression.Binding(_, expression, _, _, _) =>
        binding
          .copy(
            expression = analyseExpression(expression, isInTailPosition = false)
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case err: Diagnostic =>
        err.updateMetadata(
          this -->> TailPosition.fromBool(isInTailPosition)
        )
    }
  }

  /** Performs tail call analysis on an occurrence of a name.
    *
    * @param name the name to analyse
    * @param isInTailPosition whether the name occurs in tail position or not
    * @return `name`, annotated with tail position metadata
    */
  def analyseName(name: Name, isInTailPosition: Boolean): Name = {
    name.updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a literal.
    *
    * @param literal the literal to analyse
    * @param isInTailPosition whether or not the literal occurs in tail position
    *                         or not
    * @return `literal`, annotated with tail position metdata
    */
  def analyseLiteral(
    literal: Literal,
    isInTailPosition: Boolean
  ): Literal = {
    literal.updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on an application.
    *
    * @param application the application to analyse
    * @param isInTailPosition whether or not the application is occurring in
    *                         tail position
    * @return `application`, annotated with tail position metadata
    */
  def analyseApplication(
    application: Application,
    isInTailPosition: Boolean
  ): Application = {
    application match {
      case app @ Application.Prefix(fn, args, _, _, _, _) =>
        app
          .copy(
            function  = analyseExpression(fn, isInTailPosition = false),
            arguments = args.map(analyseCallArg)
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case force @ Application.Force(target, _, _, _) =>
        force
          .copy(
            target = analyseExpression(target, isInTailPosition)
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case vector @ Application.Sequence(items, _, _, _) =>
        vector
          .copy(items =
            items.map(analyseExpression(_, isInTailPosition = false))
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case tSet @ Application.Typeset(expr, _, _, _) =>
        tSet
          .copy(expression =
            expr.map(analyseExpression(_, isInTailPosition = false))
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case _: Operator =>
        throw new CompilerError("Unexpected binary operator.")
    }
  }

  /** Performs tail call analysis on a call site argument.
    *
    * @param argument the argument to analyse
    * @return `argument`, annotated with tail position metadata
    */
  def analyseCallArg(argument: CallArgument): CallArgument = {
    argument match {
      case arg @ CallArgument.Specified(_, expr, _, _, _) =>
        arg
          .copy(
            // Note [Call Argument Tail Position]
            value = analyseExpression(expr, isInTailPosition = true)
          )
          .updateMetadata(this -->> TailPosition.Tail)
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
  def analyseType(value: Type, isInTailPosition: Boolean): Type = {
    value
      .mapExpressions(analyseExpression(_, isInTailPosition = false))
      .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a case expression.
    *
    * @param caseExpr the case expression to analyse
    * @param isInTailPosition whether or not the case expression occurs in a tail
    *                         call position
    * @return `caseExpr`, annotated with tail position metadata
    */
  def analyseCase(caseExpr: Case, isInTailPosition: Boolean): Case = {
    caseExpr match {
      case caseExpr @ Case.Expr(scrutinee, branches, _, _, _, _) =>
        caseExpr
          .copy(
            scrutinee = analyseExpression(scrutinee, isInTailPosition = false),
            // Note [Analysing Branches in Case Expressions]
            branches = branches.map(analyseCaseBranch(_, isInTailPosition))
          )
          .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
      case _: Case.Branch =>
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
    branch: Case.Branch,
    isInTailPosition: Boolean
  ): Case.Branch = {
    branch
      .copy(
        pattern = analysePattern(branch.pattern),
        expression = analyseExpression(
          branch.expression,
          isInTailPosition
        )
      )
      .updateMetadata(this -->> TailPosition.fromBool(isInTailPosition))
  }

  /** Performs tail call analysis on a pattern.
    *
    * @param pattern the pattern to analyse
    * @return `pattern`, annotated with tail position metadata
    */
  def analysePattern(
    pattern: Pattern
  ): Pattern = {
    pattern match {
      case namePat @ Pattern.Name(name, _, _, _) =>
        namePat
          .copy(
            name = analyseName(name, isInTailPosition = false)
          )
          .updateMetadata(this -->> TailPosition.NotTail)
      case cons @ Pattern.Constructor(constructor, fields, _, _, _) =>
        cons
          .copy(
            constructor = analyseName(constructor, isInTailPosition = false),
            fields      = fields.map(analysePattern)
          )
          .updateMetadata(this -->> TailPosition.NotTail)
      case literal: Pattern.Literal =>
        literal
          .updateMetadata(this -->> TailPosition.NotTail)
      case tpePattern @ Pattern.Type(name, tpe, _, _, _) =>
        tpePattern
          .copy(
            name = analyseName(name, isInTailPosition = false),
            tpe  = analyseName(tpe, isInTailPosition = false)
          )
      case err: errors.Pattern =>
        err.updateMetadata(this -->> TailPosition.NotTail)
      case _: Pattern.Documentation =>
        throw new CompilerError(
          "Branch documentation should be desugared at an earlier stage."
        )
    }
  }

  /** Performs tail call analysis on a function definition.
    *
    * @param function the function to analyse
    * @param isInTailPosition whether or not the function definition occurs in a
    *                         tail position
    * @return `function`, annotated with tail position metadata
    */
  def analyseFunction(
    function: Function,
    isInTailPosition: Boolean
  ): Function = {
    val canBeTCO   = function.canBeTCO
    val markAsTail = (!canBeTCO && isInTailPosition) || canBeTCO

    val resultFunction = function match {
      case lambda @ Function.Lambda(args, body, _, _, _, _) =>
        lambda.copy(
          arguments = args.map(analyseDefArgument),
          body      = analyseExpression(body, isInTailPosition = markAsTail)
        )
      case _: Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during tail call analysis."
        )
    }

    resultFunction.updateMetadata(
      this -->> TailPosition.fromBool(isInTailPosition)
    )
  }

  /** Performs tail call analysis on a function definition argument.
    *
    * @param arg the argument definition to analyse
    * @return `arg`, annotated with tail position metadata
    */
  def analyseDefArgument(arg: DefinitionArgument): DefinitionArgument = {
    arg match {
      case arg @ DefinitionArgument.Specified(_, _, default, _, _, _, _) =>
        arg
          .copy(
            defaultValue = default.map(x =>
              analyseExpression(x, isInTailPosition = false)
                .updateMetadata(this -->> TailPosition.NotTail)
            )
          )
          .updateMetadata(this -->> TailPosition.NotTail)
    }
  }

  /** Expresses the tail call state of an IR Node. */
  sealed trait TailPosition extends IRPass.IRMetadata {

    /** A boolean representation of the expression's tail state. */
    def isTail: Boolean
  }
  object TailPosition {

    /** The expression is in a tail position and can be tail call optimised. */
    final case object Tail extends TailPosition {
      override val metadataName: String = "TailCall.TailPosition.Tail"
      override def isTail: Boolean      = true

      override def duplicate(): Option[IRPass.IRMetadata] = Some(Tail)

      /** @inheritdoc */
      override def prepareForSerialization(compiler: Compiler): Tail.type = this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Tail.type] = Some(this)
    }

    /** The expression is not in a tail position and cannot be tail call
      * optimised.
      */
    final case object NotTail extends TailPosition {
      override val metadataName: String = "TailCall.TailPosition.NotTail"
      override def isTail: Boolean      = false

      override def duplicate(): Option[IRPass.IRMetadata] = Some(NotTail)

      /** @inheritdoc */
      override def prepareForSerialization(compiler: Compiler): NotTail.type =
        this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[NotTail.type] = Some(this)
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

  /** Checks if the provided `expression` is annotated with a tail call
    * annotation.
    *
    * @param expression the expression to check
    * @return `true` if `expression` is annotated with `@Tail_Call`, otherwise
    *         `false`
    */
  def isTailAnnotated(expression: Expression): Boolean = {
    expression
      .getMetadata(ExpressionAnnotations)
      .exists(anns =>
        anns.annotations.exists(a =>
          a.name == ExpressionAnnotations.tailCallName
        )
      )
  }
}

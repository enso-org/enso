package org.enso.compiler.pass.analyse

import org.enso.compiler.core.Implicits.{AsDiagnostics, AsMetadata}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.{
  Application,
  Case,
  Comment,
  Foreign
}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Empty,
  Expression,
  Function,
  Module,
  Name,
  Pattern,
  Type,
  Warning
}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.MiniIRPass
import org.enso.compiler.pass.analyse.TailCall.TailPosition
import org.enso.compiler.pass.resolve.ExpressionAnnotations

import java.util.IdentityHashMap

/** @param isInTailPosition Indicates whether the current IR element should be marked as tail call.
  *                         This flag is propagated to the children during the preparation phase.
  * @param tails A set of IR elements (collected and used only during the preparation phase)
  *              that are definitely tail calls, even if `isInTailPosition` is false.
  *              Implemented as [[java.util.IdentityHashMap]] to force comparison via referential
  *              equality of the IR elements, instead of `IR.equals`. The value type in this map has
  *              no meaning.
  * @param notTails A set of IR elements (collected and used only during the preparation phase)
  *                 that are definitely not tail calls, even if `isInTailPosition` is true.
  */
class TailCallMini(
  private val isInTailPosition: Boolean = false,
  private val tails: IdentityHashMap[IR, Boolean] =
    new IdentityHashMap[IR, Boolean](),
  private val notTails: IdentityHashMap[IR, Boolean] =
    new IdentityHashMap[IR, Boolean]()
) extends MiniIRPass {

  private val tailMeta    = new MetadataPair(this, TailPosition.Tail)
  private val notTailMeta = new MetadataPair(this, TailPosition. /*Not*/ Tail)

  override type Metadata = TailCall.TailPosition

  override def transformModule(moduleIr: Module): Module = {
    moduleIr.copy(
      bindings = moduleIr.bindings.map(analyseModuleBinding)
    )
  }

  override def prepare(expr: Expression): MiniIRPass = {
    if (tails.containsKey(expr)) {
      tails.remove(expr)
      return new TailCallMini(true, tails, notTails)
    }
    if (notTails.containsKey(expr)) {
      notTails.remove(expr)
      return new TailCallMini(false, tails, notTails)
    }

    expr match {
      case binding: Expression.Binding =>
        notTails.put(binding.expression, true)
      case block: Expression.Block =>
        block.expressions.foreach { bodyExpr =>
          notTails.put(bodyExpr, true)
        }
      case Application.Prefix(fn, args, _, _, _) =>
        notTails.put(fn, true)
        // Note [Call Argument Tail Position]
        args.foreach { case CallArgument.Specified(_, value, _, _) =>
          tails.put(value, true)
        }
      case seq: Application.Sequence =>
        seq.items.foreach { item =>
          notTails.put(item, true)
        }
      case tpSet: Application.Typeset =>
        tpSet.expression.map(notTails.put(_, true))
      case Function.Lambda(args, body, _, canBeTCO, _, _) =>
        val markAsTail = (!canBeTCO && isInTailPosition) || canBeTCO
        if (markAsTail) {
          tails.put(body, true)
        } else {
          notTails.put(body, true)
        }
        args.foreach {
          case DefinitionArgument.Specified(
                _,
                _,
                Some(defaultValue),
                _,
                _,
                _
              ) =>
            notTails.put(defaultValue, true)
          case _ => ()
        }
      case tp: Type =>
        tp.children().foreach { tpChild =>
          notTails.put(tpChild, true)
        }
      case Case.Expr(scrutinee, _, _, _, _) =>
        notTails.put(scrutinee, true)
      case pat: Pattern =>
        pat.children().foreach { patChild =>
          notTails.put(patChild, true)
        }
      case _ => ()
    }
    this
  }

  override def transformExpression(ir: Expression): Expression = {
    val irWithUpdatedChildren = ir match {
      case app @ Application.Prefix(_, args, _, _, _) =>
        args.foreach { arg =>
          arg.updateMetadata(tailMeta)
        }
        app
      case lambda @ Function.Lambda(args, _, _, _, _, _) =>
        args.foreach { arg =>
          arg.updateMetadata(notTailMeta)
        }
        lambda
      case _ => ir
    }

    irWithUpdatedChildren match {
      case _: Name.GenericAnnotation =>
        ir.updateMetadata(tailMeta)
      case _: Pattern =>
        ir.updateMetadata(notTailMeta)
      case expr: Expression => analyseExpression(expr)
      case _ =>
        if (isInTailPosition) {
          ir.updateMetadata(tailMeta)
        } else {
          ir.updateMetadata(notTailMeta)
        }
    }
  }

  override def checkPostCondition(ir: IR): Boolean = {
    // If isInTail position is true, there must be TailCall metadata attached.
    if (isInTailPosition) {
      ir.getMetadata(this) match {
        case Some(meta) if meta.isTail => true
        case _                         => false
      }
    } else {
      true
    }
  }

  /** Performs tail call analysis on a top-level definition in a module.
    *
    * @param moduleDefinition the top-level definition to analyse
    * @return `definition`, annotated with tail call information
    */
  private def analyseModuleBinding(
    moduleDefinition: Definition
  ): Definition = {
    moduleDefinition match {
      case method: definition.Method.Conversion =>
        method
          .updateMetadata(new MetadataPair(TailCall, TailPosition.Tail))
      case method @ definition.Method
            .Explicit(_, _, _, _, _) =>
        method
          .updateMetadata(new MetadataPair(TailCall, TailPosition.Tail))
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during tail call " +
          "analysis."
        )
      case _: Definition.Type =>
        moduleDefinition.updateMetadata(
          new MetadataPair(TailCall, TailPosition.Tail)
        )
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
          .updateMetadata(new MetadataPair(TailCall, TailPosition.Tail))
      case err: Error => err
    }
  }

  /** Performs tail call analysis on an arbitrary expression.
    *
    * @param expression the expression to analyse
    * @return `expression`, annotated with tail position metadata
    */
  def analyseExpression(
    expression: Expression
  ): Expression = {
    val expressionWithWarning =
      if (isTailAnnotated(expression) && !isInTailPosition)
        expression.addDiagnostic(Warning.WrongTco(expression.location.orNull))
      else expression
    expressionWithWarning match {
      case empty: Empty =>
        empty.updateMetadata(notTailMeta)
      case foreign: Foreign =>
        foreign.updateMetadata(notTailMeta)
      case _: Comment =>
        throw new CompilerError(
          "Comments should not be present during tail call analysis."
        )
      case _ =>
        if (isInTailPosition) {
          expressionWithWarning.updateMetadata(tailMeta)
        } else {
          expressionWithWarning.updateMetadata(notTailMeta)
        }
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

  override def toString: String = {
    val _tails =
      tails.keySet().stream().map(k => k.getClass.getSimpleName).toList
    val _notTails =
      notTails.keySet().stream().map(k => k.getClass.getSimpleName).toList
    s"TailCallMini(isInTailPosition=$isInTailPosition, tails=${_tails}, notTails=${_notTails})"
  }

  /* from develop:
  =======
  private lazy val TAIL_META = new MetadataPair(this, TailPosition.Tail)

  /** Analyses tail call state for expressions in a module.
   *
   * @param ir the Enso IR to process
   * @param moduleContext a context object that contains the information needed
   *                      to process a module
   * @return `ir`, possibly having made transformations or annotations to that
   *         IR.
   */
>>>>>>> origin/develop
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    ???
  }

  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
<<<<<<< HEAD
  ): Expression = ???
=======
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
   * @param moduleDefinition the top-level definition to analyse
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
          .updateMetadata(TAIL_META)
      case method @ definition.Method
            .Explicit(_, body, _, _, _) =>
        method
          .copy(
            body = analyseExpression(body, isInTailPosition = true)
          )
          .updateMetadata(TAIL_META)
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during tail call " +
          "analysis."
        )
      case _: Definition.Type =>
        moduleDefinition.updateMetadata(
          TAIL_META
        )
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
          .updateMetadata(TAIL_META)
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
        expression.addDiagnostic(
          Warning.WrongTco(expression.identifiedLocation())
        )
      else expression
    expressionWithWarning match {
      case function: Function =>
        analyseFunction(function, isInTailPosition)
      case caseExpr: Case   => analyseCase(caseExpr, isInTailPosition)
      case typ: Type        => analyseType(typ, isInTailPosition)
      case app: Application => analyseApplication(app, isInTailPosition)
      case name: Name       => analyseName(name, isInTailPosition)
      case literal: Literal => analyseLiteral(literal, isInTailPosition)
      case _: Comment =>
        throw new CompilerError(
          "Comments should not be present during tail call analysis."
        )
      case block @ Expression.Block(expressions, returnValue, _, _, _) =>
        updateMetaIfInTailPosition(
          isInTailPosition,
          block
            .copy(
              expressions = expressions.map(
                analyseExpression(_, isInTailPosition = false)
              ),
              returnValue = analyseExpression(returnValue, isInTailPosition)
            )
        )
      case binding @ Expression.Binding(_, expression, _, _) =>
        updateMetaIfInTailPosition(
          isInTailPosition,
          binding
            .copy(
              expression =
                analyseExpression(expression, isInTailPosition = false)
            )
        )
      case err: Diagnostic => updateMetaIfInTailPosition(isInTailPosition, err)
      case _               => expressionWithWarning
    }
  }

  private def updateMetaIfInTailPosition[T <: IR](
    isInTailPosition: Boolean,
    ir: T
  ): T = {
    if (isInTailPosition) {
      ir.updateMetadata(TAIL_META)
    } else {
      ir
    }
  }

  /** Performs tail call analysis on an occurrence of a name.
   *
   * @param name the name to analyse
   * @param isInTailPosition whether the name occurs in tail position or not
   * @return `name`, annotated with tail position metadata
   */
  def analyseName(name: Name, isInTailPosition: Boolean): Name = {
    updateMetaIfInTailPosition(isInTailPosition, name)
  }

  /** Performs tail call analysis on a literal.
   *
   * @param literal the literal to analyse
   * @param isInTailPosition whether or not the literal occurs in tail position
   *                         or not
   * @return `literal`, annotated with tail position metdata
   */
  private def analyseLiteral(
    literal: Literal,
    isInTailPosition: Boolean
  ): Literal = {
    updateMetaIfInTailPosition(isInTailPosition, literal)
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
    val newApp = application match {
      case app @ Application.Prefix(fn, args, _, _, _) =>
        app
          .copy(
            function  = analyseExpression(fn, isInTailPosition = false),
            arguments = args.map(analyseCallArg)
          )
      case force @ Application.Force(target, _, _) =>
        force
          .copy(
            target = analyseExpression(target, isInTailPosition)
          )
      case vector @ Application.Sequence(items, _, _) =>
        vector
          .copy(items =
            items.map(analyseExpression(_, isInTailPosition = false))
          )
      case tSet @ Application.Typeset(expr, _, _) =>
        tSet
          .copy(expression =
            expr.map(analyseExpression(_, isInTailPosition = false))
          )
      case _: Operator =>
        throw new CompilerError("Unexpected binary operator.")
    }
    updateMetaIfInTailPosition(isInTailPosition, newApp)
  }

  /** Performs tail call analysis on a call site argument.
   *
   * @param argument the argument to analyse
   * @return `argument`, annotated with tail position metadata
   */
  private def analyseCallArg(argument: CallArgument): CallArgument = {
    argument match {
      case arg @ CallArgument.Specified(_, expr, _, _) =>
        arg
          .copy(
            // Note [Call Argument Tail Position]
            value = analyseExpression(expr, isInTailPosition = true)
          )
          .updateMetadata(TAIL_META)
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
    updateMetaIfInTailPosition(
      isInTailPosition,
      value
        .mapExpressions(analyseExpression(_, isInTailPosition = false))
    )
  }

  /** Performs tail call analysis on a case expression.
   *
   * @param caseExpr the case expression to analyse
   * @param isInTailPosition whether or not the case expression occurs in a tail
   *                         call position
   * @return `caseExpr`, annotated with tail position metadata
   */
  def analyseCase(caseExpr: Case, isInTailPosition: Boolean): Case = {
    val newCaseExpr = caseExpr match {
      case caseExpr @ Case.Expr(scrutinee, branches, _, _, _) =>
        caseExpr
          .copy(
            scrutinee = analyseExpression(scrutinee, isInTailPosition = false),
            // Note [Analysing Branches in Case Expressions]
            branches = branches.map(analyseCaseBranch(_, isInTailPosition))
          )
      case _: Case.Branch =>
        throw new CompilerError("Unexpected case branch.")
    }
    updateMetaIfInTailPosition(isInTailPosition, newCaseExpr)
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
    updateMetaIfInTailPosition(
      isInTailPosition,
      branch
        .copy(
          pattern = analysePattern(branch.pattern),
          expression = analyseExpression(
            branch.expression,
            isInTailPosition
          )
        )
    )
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
      case namePat @ Pattern.Name(name, _, _) =>
        namePat
          .copy(
            name = analyseName(name, isInTailPosition = false)
          )
      case cons @ Pattern.Constructor(constructor, fields, _, _) =>
        cons
          .copy(
            constructor = analyseName(constructor, isInTailPosition = false),
            fields      = fields.map(analysePattern)
          )
      case literal: Pattern.Literal => literal
      case tpePattern @ Pattern.Type(name, tpe, _, _) =>
        tpePattern
          .copy(
            name = analyseName(name, isInTailPosition = false),
            tpe  = analyseName(tpe, isInTailPosition = false)
          )
      case err: errors.Pattern => err
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
    updateMetaIfInTailPosition(isInTailPosition, resultFunction)
  }

  /** Performs tail call analysis on a function definition argument.
   *
   * @param arg the argument definition to analyse
   * @return `arg`, annotated with tail position metadata
   */
  private def analyseDefArgument(
    arg: DefinitionArgument
  ): DefinitionArgument = {
    arg match {
      case arg @ DefinitionArgument.Specified(_, _, default, _, _, _) =>
        arg
          .copy(
            defaultValue =
              default.map(x => analyseExpression(x, isInTailPosition = false))
          )
    }
  }
>>>>>>> origin/develop

  /** Expresses the tail call state of an IR Node. */
  sealed trait TailPosition extends IRPass.IRMetadata {

    /** A boolean representation of the expression's tail state. */
    def isTail: Boolean
  }
  object TailPosition {

    /** The expression is in a tail position and can be tail call optimised.
   * If the expression is not in tail-call position, it has no metadata attached.
   */
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
   */
}

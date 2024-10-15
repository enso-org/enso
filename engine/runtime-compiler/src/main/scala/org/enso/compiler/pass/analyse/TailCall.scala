package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.{AsDiagnostics, AsMetadata}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.{
  Application,
  Case,
  Comment,
  Error,
  Operator
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.{
  CallArgument,
  Expression,
  Function,
  Literal,
  Module,
  Name,
  ProcessingPass,
  Type,
  Warning
}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.MiniPassFactory
import org.enso.compiler.pass.MiniIRPass
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.resolve.{ExpressionAnnotations, GlobalNames}

/** This pass performs tail call analysis on the Enso IR.
  *
  * It is responsible for marking every single expression with whether it is in
  * tail position. This allows the code generator to correctly create the
  * Truffle nodes.
  * If the expression is in tail position, [[TailPosition.Tail]] metadata is attached
  * to it, otherwise, nothing is attached.
  *
  * This pass requires the context to provide:
  *
  * - The tail position of its expression, where relevant.
  */
case object TailCall extends MiniPassFactory with ProcessingPass {

  /** The annotation metadata type associated with IR nodes by this pass. */
  override type Metadata = TailPosition

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp.INSTANCE,
    OperatorToFunction,
    LambdaShorthandToLambda,
    GlobalNames
  )

  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List()

  private lazy val TAIL_META = new MetadataPair(this, TailPosition.Tail)

  override def createForInlineCompilation(
    inlineContext: InlineContext
  ): MiniIRPass = {
    val isInTailPos = inlineContext.isInTailPosition.getOrElse(
      throw new CompilerError(
        "Information about the tail position for an inline expression " +
        "must be known by the point of tail call analysis."
      )
    )
    new Mini(isInTailPos)
  }

  override def createForModuleCompilation(
    moduleContext: ModuleContext
  ): MiniIRPass = {
    new Mini(false)
  }

  /** Expresses the tail call state of an IR Node. */
  sealed trait TailPosition extends IRPass.IRMetadata {

    /** A boolean representation of the expression's tail state. */
    def isTail: Boolean
  }
  object TailPosition {

    /** The expression is in a tail position and can be tail call optimised.
      * If the expression is not in tail-call position, it has no metadata attached.
      */
    case object Tail extends TailPosition {
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
  final def isTailAnnotated(expression: Expression): Boolean = {
    expression
      .getMetadata(ExpressionAnnotations)
      .exists(anns =>
        anns.annotations.exists(a =>
          a.name == ExpressionAnnotations.tailCallName
        )
      )
  }

  private class Mini(private val isInTailPos: Boolean) extends MiniIRPass {
    private val tailCandidates = {
      val map = new java.util.IdentityHashMap[IR, Boolean]();
      if (isInTailPos) {
        map.put(null, true)
      }
      map
    }

    override def transformModule(m: Module): Module = {
      m.bindings.map(subTailModuleBinding)
      m
    }

    override def prepare(parent: IR, child: Expression): Mini = {
      parent match {
        case m: Module => m.bindings.map(subTailModuleBinding)
        case e: Expression =>
          if (tailCandidates.remove(null)) {
            subTailExpression(e, true)
            e.updateMetadata(TAIL_META)
          }
        case _ =>
      }
      val tailStatus = isTailPosition(child)
      subTailExpression(child, tailStatus)
      if (!tailStatus && isTailAnnotated(child)) {
        child.addDiagnostic(
          Warning.WrongTco(child.identifiedLocation())
        )
      }
      this
    }

    override def transformExpression(ir: Expression): Expression = {
      ir
    }

    private def isTailPosition(ir: Expression): Boolean = {
      if (tailCandidates.containsKey(ir)) {
        tailCandidates.get(ir)
      } else {
        // if null is in the candidates, replace it with first Expression
        val initialState = tailCandidates.remove(null)
        if (initialState) {
          tailCandidates.put(ir, true)
          ir.updateMetadata(TAIL_META)
          initialState
        } else {
          false
        }
      }
    }

    /** Performs tail call analysis on a top-level definition in a module.
      *
      * @param moduleDefinition the top-level definition to analyse
      * @return `definition`, annotated with tail call information
      */
    private def subTailModuleBinding(
      moduleDefinition: Definition
    ): Unit = {
      moduleDefinition match {
        case method: definition.Method.Conversion =>
          tailCandidates.put(method.body, true)
          method.updateMetadata(TAIL_META)
        case method @ definition.Method.Explicit(_, body, _, _, _) =>
          tailCandidates.put(body, true)
          method.updateMetadata(TAIL_META)
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
          tailCandidates.put(ann.expression, true)
          ann.updateMetadata(TAIL_META)
        case _: Error =>
      }
    }

    /** Performs tail call analysis on an arbitrary expression.
      *
      * @param expression the expression to subTail
      * @param isInTailPosition whether or not the expression is occurring in tail
      *                         position
      * @return `expression`, annotated with tail position metadata
      */
    private def subTailExpression(
      expression: Expression,
      isInTailPosition: Boolean
    ): Unit = {
      expression match {
        case function: Function =>
          subTailFunction(function, isInTailPosition)
        case caseExpr: Case   => subTailCase(caseExpr, isInTailPosition)
        case typ: Type        => subTailType(typ, isInTailPosition)
        case app: Application => subTailApplication(app, isInTailPosition)
        case name: Name       => subTailName(name, isInTailPosition)
        case literal: Literal => subTailLiteral(literal, isInTailPosition)
        case _: Comment =>
          throw new CompilerError(
            "Comments should not be present during tail call analysis."
          )
        case Expression.Block(_, returnValue, _, _, _) =>
          if (isInTailPosition) {
            expression.updateMetadata(TAIL_META)
            tailCandidates.put(returnValue, true)
          }
        case _ =>
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
      * @param name the name to subTail
      * @param isInTailPosition whether the name occurs in tail position or not
      * @return `name`, annotated with tail position metadata
      */
    def subTailName(name: Name, isInTailPosition: Boolean): Name = {
      updateMetaIfInTailPosition(isInTailPosition, name)
    }

    /** Performs tail call analysis on a literal.
      *
      * @param literal the literal to subTail
      * @param isInTailPosition whether or not the literal occurs in tail position
      *                         or not
      * @return `literal`, annotated with tail position metdata
      */
    private def subTailLiteral(
      literal: Literal,
      isInTailPosition: Boolean
    ): Literal = {
      updateMetaIfInTailPosition(isInTailPosition, literal)
    }

    /** Performs tail call analysis on an application.
      *
      * @param application the application to subTail
      * @param isInTailPosition whether or not the application is occurring in
      *                         tail position
      * @return `application`, annotated with tail position metadata
      */
    private def subTailApplication(
      application: Application,
      isInTailPosition: Boolean
    ): Unit = {
      if (isInTailPosition) {
        application.updateMetadata(TAIL_META)
      }
      application match {
        case Application.Prefix(_, args, _, _, _) =>
          args.foreach(subTailCallArg)
        case Application.Force(target, _, _) =>
          if (isInTailPosition) {
            tailCandidates.put(target, true)
          }
        case Application.Sequence(_, _, _) =>
        case Application.Typeset(_, _, _)  =>
        case _: Operator =>
          throw new CompilerError("Unexpected binary operator.")
      }
    }

    /** Performs tail call analysis on a call site argument.
      *
      * @param argument the argument to subTail
      * @return `argument`, annotated with tail position metadata
      */
    private def subTailCallArg(argument: CallArgument): Unit = {
      argument match {
        case CallArgument.Specified(_, expr, _, _) =>
          // Note [Call Argument Tail Position]
          tailCandidates.put(expr, true)
      }
      argument.updateMetadata(TAIL_META)
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
    private def subTailType(value: Type, isInTailPosition: Boolean): Unit = {}

    /** Performs tail call analysis on a case expression.
      *
      * @param caseExpr the case expression to subTail
      * @param isInTailPosition whether or not the case expression occurs in a tail
      *                         call position
      * @return `caseExpr`, annotated with tail position metadata
      */
    private def subTailCase(caseExpr: Case, isInTailPosition: Boolean): Unit = {
      if (isInTailPosition) {
        caseExpr.updateMetadata(TAIL_META)
      }
      caseExpr match {
        case Case.Expr(_, branches, _, _, _) =>
          if (isInTailPosition) {
            // Note [Analysing Branches in Case Expressions]
            branches.foreach(b => {
              b.updateMetadata(TAIL_META)
              tailCandidates.put(b.expression, true)
            })
          }
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

    /** Branch expression may be in tail position.
      *
      * @param branch the branch to subTail
      * @param isInTailPosition whether or not the branch occurs in a tail call
      *                         position
      * @return `branch`, annotated with tail position metadata
      *    private def subTailCaseBranch(
      *      branch: Case.Branch,
      *      isInTailPosition: Boolean
      *      ): Unit = {
      *        if (isInTailPosition) {
      *          tailCandidates.put(branch.expression, isInTailPosition)
      *        }
      *      }
      */

    /** Patterns are never in tail possition.
      *
      * @param pattern the pattern to subTail
      * @return `pattern`, annotated with tail position metadata
      *    private def subTailPattern(
      *      pattern: Pattern
      *      ): Unit = {
      *      }
      */

    /** Body of the function may be in tail position.
      *
      * @param function the function to subTail
      * @param isInTailPosition whether or not the function definition occurs in a
      *                         tail position
      * @return `function`, annotated with tail position metadata
      */
    private def subTailFunction(
      function: Function,
      isInTailPosition: Boolean
    ): Unit = {
      val canBeTCO   = function.canBeTCO
      val markAsTail = (!canBeTCO && isInTailPosition) || canBeTCO
      if (isInTailPosition) {
        function.updateMetadata(TAIL_META)
      }
      function match {
        case Function.Lambda(_, body, _, _, _, _) =>
          if (markAsTail) {
            tailCandidates.put(body, true)
          }
        case _: Function.Binding =>
          throw new CompilerError(
            "Function sugar should not be present during tail call analysis."
          )
      }
    }

    /** No tail expressions in default arguments. Body of the function is next.
      *   private def subTailDefArgument(
      */
  }
}

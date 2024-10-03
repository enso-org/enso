package org.enso.compiler.pass.analyse

import org.enso.compiler.core.Implicits.{AsDiagnostics, AsMetadata}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.{
  Application,
  Case,
  Comment,
  Foreign
}
import org.enso.compiler.core.ir.module.scope.{definition, Definition}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Empty,
  Expression,
  Function,
  Name,
  Pattern,
  Type,
  Warning
}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.MiniIRPass
import org.enso.compiler.pass.analyse.TailCall.TailPosition
import org.enso.compiler.pass.resolve.ExpressionAnnotations
import org.graalvm.collections.{EconomicSet, Equivalence}

/** @param isInTailPosition Indicates whether the current IR element should be marked as tail call.
  *                         This flag is propagated to the children during the preparation phase.
  * @param tails A set of IR elements (collected and used only during the preparation phase)
  *                         that are definitely tail calls, even if `isInTailPosition` is false.
  *              EconomitSet with IDENTITY_SYSTEM_HASHCODE is chosen to the comparison of
  *              references rather than via `IR.equals` and `IR.hashCode`.
  * @param notTails A set of IR elements (collected and used only during the preparation phase)
  *                 that are definitely not tail calls, even if `isInTailPosition` is true.
  */
class TailCallMini(
  private val isInTailPosition: Boolean = false,
  private val tails: EconomicSet[IR] =
    EconomicSet.create(Equivalence.IDENTITY_WITH_SYSTEM_HASHCODE),
  private val notTails: EconomicSet[IR] =
    EconomicSet.create(Equivalence.IDENTITY_WITH_SYSTEM_HASHCODE)
) extends MiniIRPass {
  override type Metadata = TailCall.TailPosition

  override def prepare(current: IR): MiniIRPass = {
    if (tails.contains(current)) {
      tails.remove(current)
      return new TailCallMini(true, tails, notTails)
    }
    if (notTails.contains(current)) {
      notTails.remove(current)
      return new TailCallMini(false, tails, notTails)
    }

    current match {
      case binding: Expression.Binding =>
        notTails.add(binding.expression)
      case block: Expression.Block =>
        block.expressions.foreach { bodyExpr =>
          notTails.add(bodyExpr)
        }
      case Application.Prefix(fn, _, _, _, _) if current eq fn =>
        notTails.add(fn)
      case seq: Application.Sequence =>
        seq.items.foreach { item =>
          notTails.add(item)
        }
      case tpSet: Application.Typeset =>
        tpSet.expression.map(notTails.add)
      case Function.Lambda(_, body, _, canBeTCO, _, _) if current eq body =>
        val markAsTail = (!canBeTCO && isInTailPosition) || canBeTCO
        if (markAsTail) {
          tails.add(body)
        } else {
          notTails.add(body)
        }
      // Note [Call Argument Tail Position]
      case CallArgument.Specified(_, value, _, _) =>
        tails.add(value)
      case DefinitionArgument.Specified(_, _, Some(defaultValue), _, _, _) =>
        notTails.add(defaultValue)
      case tp: Type =>
        tp.children().foreach { tpChild =>
          notTails.add(tpChild)
        }
      case Case.Expr(scrutinee, _, _, _, _) =>
        notTails.add(scrutinee)
      case pat: Pattern =>
        pat.children().foreach { patChild =>
          notTails.add(patChild)
        }
      case _ => ()
    }
    this
  }

  override def transformIr(ir: IR): IR = {
    val tailMeta    = new MetadataPair(this, TailPosition.Tail)
    val notTailMeta = new MetadataPair(this, TailPosition.NotTail)
    ir match {
      case _: CallArgument.Specified =>
        ir.updateMetadata(tailMeta)
      case _: DefinitionArgument.Specified =>
        ir.updateMetadata(notTailMeta)
      case _: definition.Method.Conversion =>
        ir.updateMetadata(tailMeta)
      case _: definition.Method.Explicit =>
        ir.updateMetadata(tailMeta)
      case _: Definition.Type =>
        ir.updateMetadata(tailMeta)
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

  /** Performs tail call analysis on an arbitrary expression.
    *
    * @param expression the expression to analyse
    * @return `expression`, annotated with tail position metadata
    */
  def analyseExpression(
    expression: Expression
  ): Expression = {
    val tailMeta    = new MetadataPair(this, TailPosition.Tail)
    val notTailMeta = new MetadataPair(this, TailPosition.NotTail)
    val expressionWithWarning =
      if (isTailAnnotated(expression) && !isInTailPosition)
        expression.addDiagnostic(Warning.WrongTco(expression.location))
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
}

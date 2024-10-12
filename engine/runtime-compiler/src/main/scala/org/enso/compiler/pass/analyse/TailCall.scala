package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.resolve.{ExpressionAnnotations, GlobalNames}
import org.enso.compiler.pass.{IRPass, MiniPassFactory}

/** Implementation moved to [[org.enso.compiler.test.pass.analyse.TailCallMegaPass]] test
  */
case object TailCall extends IRPass with MiniPassFactory {

  /** The annotation metadata type associated with IR nodes by this pass. */
  override type Metadata = TailPosition

  override type Config = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp,
    OperatorToFunction,
    LambdaShorthandToLambda,
    GlobalNames
  )

  override lazy val invalidatedPasses: Seq[IRPass] = List()

  override def createForInlineCompilation(
    inlineContext: InlineContext
  ): TailCallMini = {
    val isInTailPos = inlineContext.isInTailPosition.getOrElse(
      throw new CompilerError(
        "Information about the tail position for an inline expression " +
        "must be known by the point of tail call analysis."
      )
    )
    new TailCallMini(isInTailPos)
  }

  override def createForModuleCompilation(
    moduleContext: ModuleContext
  ): TailCallMini = {
    new TailCallMini()
  }

  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    ???
  }

  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ???

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
}

package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{Expression, Module}
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

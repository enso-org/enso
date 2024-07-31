package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{
  CompilerContext,
  FramePointer,
  InlineContext,
  ModuleContext
}
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.{Expression, ProcessingPass}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRPass.IRMetadata

/** This pass attaches [[FramePointer]] as metadata to all the IR elements that already
  * have [[org.enso.compiler.pass.analyse.alias.Info.Occurrence]] attached.
  */
case object FramePointerAnalysis extends IRPass {

  override type Metadata = FramePointerMeta

  override type Config = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = {
    Seq(AliasAnalysis)
  }

  override val invalidatedPasses: Seq[IRPass] = Seq(this)

  override def runModule(ir: Module, moduleContext: ModuleContext): Module = {
    ir
  }

  /** Not implemented for this pass.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    ir
  }

  // === Pass Configuration ===================================================

  class FramePointerMeta(
    val framePointer: FramePointer
  ) extends IRMetadata {
    override val metadataName: String = "FramePointer"

    /** @inheritdoc
      */
    override def duplicate(): Option[Metadata] = {
      Some(new FramePointerMeta(framePointer))
    }

    /** @inheritdoc
      */
    override def prepareForSerialization(
      compiler: CompilerContext
    ): ProcessingPass.Metadata = this

    /** @inheritdoc
      */
    override def restoreFromSerialization(
      compiler: CompilerContext
    ): Option[ProcessingPass.Metadata] = Some(this)
  }
}

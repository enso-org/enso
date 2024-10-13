package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.pass.{IRPass, MiniPassFactory}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis
}

/** Implementation moved to `OperatorToFunctionTest`
  */
case object OperatorToFunction extends IRPass with MiniPassFactory {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    GenerateMethodBodies,
    SectionsToBinOp.INSTANCE
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis
  )

  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    ???
  }

  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    ???
  }

  override def createForModuleCompilation(
    moduleContext: ModuleContext
  ): OperatorToFunctionMini =
    new OperatorToFunctionMini()

  override def createForInlineCompilation(
    inlineContext: InlineContext
  ): OperatorToFunctionMini =
    new OperatorToFunctionMini()
}

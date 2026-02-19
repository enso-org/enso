package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.pass.{IRProcessingPass, MiniPassFactory}
import org.enso.compiler.pass.analyse.{AliasAnalysis, DemandAnalysis}

/** Implementation moved to `OperatorToFunctionTest`
  */
case object OperatorToFunction extends MiniPassFactory {

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    GenerateMethodBodies,
    SectionsToBinOp.INSTANCE
  )
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    DemandAnalysis,
    LambdaShorthandToLambda
  )

  override def createForModuleCompilation(
    moduleContext: ModuleContext
  ): OperatorToFunctionMini =
    new OperatorToFunctionMini()

  override def createForInlineCompilation(
    inlineContext: InlineContext
  ): OperatorToFunctionMini =
    new OperatorToFunctionMini()
}

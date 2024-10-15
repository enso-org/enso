package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  IgnoredBindings,
  OverloadsResolution
}
import org.enso.compiler.pass.{IRProcessingPass, MiniPassFactory}

/** Implementation moved to `LambdaShorthandToLambdaMegaPass` test.
  */
case object LambdaShorthandToLambda extends MiniPassFactory {
  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    OperatorToFunction,
    SectionsToBinOp.INSTANCE
  )
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    IgnoredBindings,
    LambdaConsolidate,
    OverloadsResolution,
    TailCall.INSTANCE,
    UnusedBindings
  )

  override def createForModuleCompilation(
    moduleContext: ModuleContext
  ): LambdaShorthandToLambdaMini = {
    val freshNameSupply = moduleContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "Desugaring underscore arguments to lambdas requires a fresh name " +
        "supply."
      )
    )
    new LambdaShorthandToLambdaMini(freshNameSupply)
  }

  override def createForInlineCompilation(
    inlineContext: InlineContext
  ): LambdaShorthandToLambdaMini = {
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "Desugaring underscore arguments to lambdas requires a fresh name " +
        "supply."
      )
    )
    new LambdaShorthandToLambdaMini(freshNameSupply)
  }
}

package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.{Expression, Module}
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
import org.enso.compiler.pass.{IRPass, MiniPassFactory}

/** Implementation moved to `LambdaShorthandToLambdaMegaPass` test.
  */
case object LambdaShorthandToLambda extends IRPass with MiniPassFactory {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    DocumentationComments,
    FunctionBinding,
    GenerateMethodBodies,
    OperatorToFunction,
    SectionsToBinOp.INSTANCE
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    IgnoredBindings,
    LambdaConsolidate,
    OverloadsResolution,
    TailCall,
    UnusedBindings
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

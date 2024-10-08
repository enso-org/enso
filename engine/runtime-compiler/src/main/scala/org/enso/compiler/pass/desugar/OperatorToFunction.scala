package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.pass.{IRPass, MiniPassFactory}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis
}

/** This pass converts usages of operators to calls to standard functions.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object OperatorToFunction extends IRPass with MiniPassFactory {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRPass] = List(
    GenerateMethodBodies,
    SectionsToBinOp
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis
  )

  /** Executes the conversion pass.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    if (!java.lang.Boolean.getBoolean("testing.mini.passes")) {
      throw new IllegalStateException(
        "OperatorToFunction.runModule should no longer be used. This is a mini pass!"
      )
    }
    val new_bindings = ir.bindings.map { a =>
      a.mapExpressions(
        runExpression(
          _,
          new InlineContext(
            moduleContext,
            compilerConfig = moduleContext.compilerConfig
          )
        )
      )
    }
    ir.copy(bindings = new_bindings)
  }

  /** Executes the conversion pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    if (!java.lang.Boolean.getBoolean("testing.mini.passes")) {
      throw new IllegalStateException(
        "OperatorToFunction.runExpression should no longer be used. This is a mini pass!"
      )
    }
    ir.transformExpressions { case operatorBinary: Operator.Binary =>
      new Application.Prefix(
        operatorBinary.operator,
        List(
          operatorBinary.left.mapExpressions(runExpression(_, inlineContext)),
          operatorBinary.right.mapExpressions(runExpression(_, inlineContext))
        ),
        hasDefaultsSuspended = false,
        operatorBinary.location,
        operatorBinary.passData,
        operatorBinary.diagnostics
      )
    }
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

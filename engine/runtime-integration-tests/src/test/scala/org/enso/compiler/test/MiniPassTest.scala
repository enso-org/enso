package org.enso.compiler.test

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.EnsoParser
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.pass.{
  IRPass,
  MiniPassFactory,
  MiniPassTraverser,
  PassManager
}

trait MiniPassTest extends CompilerTest {
  def testName: String

  /** Configuration for mini pass
    */
  def miniPassFactory: MiniPassFactory

  def megaPass: IRPass

  /** Configuration for mega pass
    */
  def megaPassManager: PassManager

  /** Tests module compilation in both mega pass and mini pass.
    * @param code Source code of the whole module to compile.
    * @param createContext Function that creates module context. For both mega pass and minipass,
    *                      there will be a new context created.
    * @param testSpec Body of the test. Receives module compiled either by mega pass or by mini pass.
    */
  def testModuleCompilation(
    code: String,
    createContext: () => ModuleContext,
    testSpec: Module => Unit
  ): Unit = {
    withClue("Mega pass module compilation") {
      val ctx      = createContext()
      val moduleIr = processModuleWithMegaPass(code, ctx)
      testSpec(moduleIr)
    }
    withClue("Mini pass module compilation") {
      val ctx      = createContext()
      val moduleIr = processModuleWithMiniPass(code, ctx)
      testSpec(moduleIr)
    }
  }

  /** Tests inline compilation in both mega pass and mini pass.
    * @param code Source code to compile.
    * @param createContext Function that creates inline context. For both mega pass and minipass,
    *                      there will be a new context created.
    * @param testSpec Body of the test. Receives expression compiled either by mega pass or by mini pass.
    */
  def testInlineCompilation(
    code: String,
    createContext: () => InlineContext,
    testSpec: Expression => Unit
  ): Unit = {
    withClue("Mega pass inline compilation: ") {
      val ctx          = createContext()
      val expressionIr = preprocessExpressionWithMegaPass(code, ctx)
      testSpec(expressionIr)
    }
    withClue("Mini pass inline compilation: ") {
      val ctx          = createContext()
      val expressionIr = preprocessExpressionWithMiniPass(code, ctx)
      testSpec(expressionIr)
    }
  }

  private def processModuleWithMegaPass(
    source: String,
    moduleCtx: ModuleContext
  ): Module = {
    val module = parseModule(source)
    val preprocessedModule =
      megaPassManager.runPassesOnModule(module, moduleCtx)
    megaPass.runModule(preprocessedModule, moduleCtx)
  }

  private def processModuleWithMiniPass(
    source: String,
    moduleCtx: ModuleContext
  ): Module = {
    val module   = parseModule(source)
    val miniPass = miniPassFactory.createForModuleCompilation(moduleCtx)
    MiniPassTraverser.compileModuleWithMiniPass(module, miniPass)
  }

  def preprocessExpressionWithMegaPass(
    expression: String,
    inlineCtx: InlineContext
  ): Expression = {
    val expr = parseExpression(expression)
    megaPassManager.runPassesInline(expr, inlineCtx)
  }

  def preprocessExpressionWithMiniPass(
    expression: String,
    inlineCtx: InlineContext
  ): Expression = {
    val expr     = parseExpression(expression)
    val miniPass = miniPassFactory.createForInlineCompilation(inlineCtx)
    MiniPassTraverser.compileInlineWithMiniPass(expr, miniPass)
  }

  private def parseModule(source: String): Module = {
    val compiler = new EnsoParser()
    try compiler.compile(source)
    finally compiler.close()
  }

  private def parseExpression(source: String): Expression = {
    val compiler = new EnsoParser()
    try {
      val exprIrOpt = compiler.generateIRInline(compiler.parse(source))
      exprIrOpt shouldBe defined
      exprIrOpt.get
    } finally compiler.close()
  }
}

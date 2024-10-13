package org.enso.compiler.test

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.EnsoParser
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.pass.{IRPass, MiniIRPass, MiniPassFactory, PassManager}

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
  def assertModuleCompilation(
    code: String,
    createContext: () => ModuleContext,
    testSpec: Module => Unit,
    compareIR: Boolean = false
  ): Unit = {
    val megaIr = withClue("Mega pass module compilation") {
      val ctx = createContext()
      processModuleWithMegaPass(code, ctx)
    }
    val miniIr = withClue("Mini pass module compilation") {
      val ctx = createContext()
      processModuleWithMiniPass(code, ctx)
    }
    if (compareIR) {
      CompilerTests.assertIR("Should be the same", megaIr, miniIr)
    }
    withClue("Mega pass module spec execution") {
      testSpec(megaIr)
    }
    withClue("Mini pass module spec execution") {
      testSpec(miniIr)
    }
  }

  /** Tests inline compilation in both mega pass and mini pass.
    * @param code Source code to compile.
    * @param createContext Function that creates inline context. For both mega pass and minipass,
    *                      there will be a new context created.
    * @param testSpec Body of the test. Receives expression compiled either by mega pass or by mini pass.
    */
  def assertInlineCompilation(
    code: String,
    createContext: () => InlineContext,
    testSpec: Expression => Unit,
    compareIR: Boolean = false
  ): Unit = {
    val megaIr = withClue("Mega pass inline compilation: ") {
      val ctx = createContext()
      preprocessExpressionWithMegaPass(code, ctx)
    }
    val miniIr = withClue("Mini pass inline compilation: ") {
      val ctx = createContext()
      preprocessExpressionWithMiniPass(code, ctx)
    }
    if (compareIR) {
      CompilerTests.assertIR("Should be the same", megaIr, miniIr)
    }
    withClue("Mega pass inline spec execution") {
      testSpec(megaIr)
    }
    withClue("Mini pass inline spec execution") {
      testSpec(miniIr)
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
    val preprocessedModule =
      megaPassManager.runPassesOnModule(module, moduleCtx)
    MiniIRPass.compile(classOf[Module], preprocessedModule, miniPass)
  }

  def preprocessExpressionWithMegaPass(
    expression: String,
    inlineCtx: InlineContext
  ): Expression = {
    val expr = parseExpression(expression)
    val preprocessedExpr =
      megaPassManager.runPassesInline(expr, inlineCtx)
    megaPass.runExpression(preprocessedExpr, inlineCtx)
  }

  def preprocessExpressionWithMiniPass(
    expression: String,
    inlineCtx: InlineContext
  ): Expression = {
    val expr     = parseExpression(expression)
    val miniPass = miniPassFactory.createForInlineCompilation(inlineCtx)
    val preprocessedExpr =
      megaPassManager.runPassesInline(expr, inlineCtx)
    MiniIRPass.compile(classOf[Expression], preprocessedExpr, miniPass)
  }

  private def parseModule(source: String): Module = {
    EnsoParser.compile(source)
  }

  private def parseExpression(source: String): Expression = {
    val exprIrOpt = EnsoParser.compileInline(source)
    exprIrOpt shouldBe defined
    exprIrOpt.get
  }
}

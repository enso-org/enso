package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap.{
  ModuleReference,
  Resolution,
  ResolvedModule
}
import org.enso.compiler.pass.resolve.MethodReferences
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.phase.ExportsResolution
import org.enso.compiler.test.CompilerTest

class MethodReferencesTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val group1 = passes.moduleDiscoveryPasses
  val group2 = new PassGroup(
    passes.globalTypingPasses.passes ++
    passes.functionBodyPasses.passes.takeWhile(_ != MethodReferences)
  )

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(group1, group2), passConfiguration)

  implicit class AnalyseModule(ir: IR.Module) {
    def analyse(implicit context: ModuleContext) = {
      MethodReferences.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Local method definition resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val code         = """
                 |main =
                 |    x1 = constant
                 |    x2 = add_one 1
                 |    y = add_one
                 |    x3 = y 1
                 |    x4 = foo 10
                 |    0
                 |
                 |constant = 2
                 |
                 |add_one x = x + 1
                 |
                 |""".stripMargin
    val parsed       = code.toIrModule
    val moduleMapped = passManager.runPassesOnModule(parsed, ctx, group1)
    ctx.module.unsafeSetIr(moduleMapped)

    new ExportsResolution().run(List(ctx.module))
    val allPrecursors = passManager.runPassesOnModule(moduleMapped, ctx, group2)
    val ir            = allPrecursors.analyse

    val bodyExprs = ir
      .bindings(0)
      .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
      .body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Expression.Block]
      .expressions
      .map(expr => expr.asInstanceOf[IR.Expression.Binding].expression)

    "resolve method names to applications" in {
      val expr = bodyExprs(0)
      expr shouldBe an[IR.Application.Prefix]
      val app = expr.asInstanceOf[IR.Application.Prefix]
      app.function.asInstanceOf[IR.Name.Literal].name shouldEqual "constant"
      app.arguments.length shouldEqual 1
      app.arguments(0).value.getMetadata(MethodReferences) shouldEqual Some(
        Resolution(ResolvedModule(ModuleReference.Concrete(ctx.module)))
      )
    }

    "resolve method names in applications by adding the self argument" in {
      val expr = bodyExprs(1)
      expr shouldBe an[IR.Application.Prefix]
      val app = expr.asInstanceOf[IR.Application.Prefix]
      app.function.asInstanceOf[IR.Name.Literal].name shouldEqual "add_one"
      app.arguments.length shouldEqual 2
      app.arguments(0).value.getMetadata(MethodReferences) shouldEqual Some(
        Resolution(ResolvedModule(ModuleReference.Concrete(ctx.module)))
      )
    }

    "resolve method names in partial applications by adding the self argument" in {
      val expr = bodyExprs(2)
      expr shouldBe an[IR.Application.Prefix]
      val app = expr.asInstanceOf[IR.Application.Prefix]
      app.function.asInstanceOf[IR.Name.Literal].name shouldEqual "add_one"
      app.arguments.length shouldEqual 1
      app.arguments(0).value.getMetadata(MethodReferences) shouldEqual Some(
        Resolution(ResolvedModule(ModuleReference.Concrete(ctx.module)))
      )
    }

    "not indicate resolution failures" in {
      val app = bodyExprs(4).asInstanceOf[IR.Application.Prefix]
      app.function.asInstanceOf[IR.Name.Literal].name shouldEqual "foo"
      app.arguments.length shouldEqual 1
      app
        .arguments(0)
        .value
        .asInstanceOf[IR.Literal.Number]
        .value shouldEqual "10"
    }
  }
}

package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.resolve.ModuleMethodThis
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ModuleMethodThisTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes

  val precursorPasses: PassGroup =
    passes.getPrecursors(ModuleMethodThis).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis         -->> AliasAnalysis.Configuration(),
    ApplicationSaturation -->> ApplicationSaturation.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext) = {
      ModuleMethodThis.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "This to here desugaring" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type Foo a b c
        |
        |Foo.method =
        |    x = this * this + this
        |    y = case this of
        |        A -> this * here
        |    z = y -> this + y
        |
        |method =
        |    x = this * this + this
        |    y = case this of
        |        A -> this * here
        |    z = y -> this + y
        |""".stripMargin.preprocessModule.analyse

    "desugar this to here in module methods" in {
      val method2 = ir
        .bindings(2)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children = method2.preorder
      val thises   = children.collect { case n: IR.Name.This => n }
      val heres    = children.collect { case n: IR.Name.Here => n }
      thises.length shouldEqual 0
      heres.length shouldEqual 7
    }

    "leave occurences of this and here untouched in non-module methods" in {
      val method1 = ir
        .bindings(1)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children = method1.preorder
      val thises   = children.collect { case n: IR.Name.This => n }
      val heres    = children.collect { case n: IR.Name.Here => n }
      thises.length shouldEqual 6
      heres.length shouldEqual 1
    }

  }
}

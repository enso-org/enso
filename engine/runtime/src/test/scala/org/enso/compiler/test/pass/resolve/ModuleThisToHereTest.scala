package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.resolve.ModuleThisToHere
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ModuleThisToHereTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(ModuleThisToHere).get

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
      ModuleThisToHere.runModule(ir, context)
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
        |    x = self * self + self
        |    y = case self of
        |        A -> self * here
        |    z = y -> self + y
        |
        |method =
        |    x = self * self + self
        |    y = case self of
        |        A -> self * here
        |    z = y -> self + y
        |
        |from (that : Foo) =
        |    x = self * self + self
        |    y = case self of
        |        A -> self * here
        |    z = y -> self + y
        |
        |Foo.from (that : Foo) =
        |    x = self * self + self
        |    y = case self of
        |        A -> self * here
        |    z = y -> self + y
        |""".stripMargin.preprocessModule.analyse

    "desugar self to here in module methods" in {
      val method2 = ir
        .bindings(2)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children        = method2.preorder
      val selfOccurrences = children.collect { case n: IR.Name.Self => n }
      val hereOccurrences = children.collect { case n: IR.Name.Here => n }
      selfOccurrences.length shouldEqual 0
      hereOccurrences.length shouldEqual 7
    }

    "leave occurrences of self and here untouched in non-module methods" in {
      val method1 = ir
        .bindings(1)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children        = method1.preorder
      val selfOccurrences = children.collect { case n: IR.Name.Self => n }
      val hereOccurrences = children.collect { case n: IR.Name.Here => n }
      selfOccurrences.length shouldEqual 6
      hereOccurrences.length shouldEqual 1
    }

    "desugar self to here in module conversions" in {
      val conv1 = ir
        .bindings(3)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Conversion]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children        = conv1.preorder
      val selfOccurrences = children.collect { case n: IR.Name.Self => n }
      val hereOccurrences = children.collect { case n: IR.Name.Here => n }
      selfOccurrences.length shouldEqual 0
      hereOccurrences.length shouldEqual 7
    }

    "leave occurrences of self and here untouched in non-module conversions" in {
      val conv2 = ir
        .bindings(4)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Conversion]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
      val children        = conv2.preorder
      val selfOccurrences = children.collect { case n: IR.Name.Self => n }
      val hereOccurrences = children.collect { case n: IR.Name.Here => n }
      selfOccurrences.length shouldEqual 6
      hereOccurrences.length shouldEqual 1
    }
  }
}

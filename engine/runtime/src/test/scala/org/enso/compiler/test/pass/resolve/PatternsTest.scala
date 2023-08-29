package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.resolve.Patterns
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class PatternsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(Patterns).get

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
  implicit class AnalyseModule(ir: Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext) = {
      Patterns.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Pattern resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type F
        |    Foo a b c
        |
        |main = case this of
        |    F.Foo a b c -> a + b + c
        |    F.Foo a b -> a + b
        |    Does_Not_Exist x y z -> x + y + z
        |    x : F -> x.a + x.b + x.c
        |
        |""".stripMargin.preprocessModule.analyse

    "attach resolved atoms to the method definitions" in {
      val patterns = ir
        .bindings(1)
        .asInstanceOf[Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]
        .branches
        .map(_.pattern)
      patterns(0)
        .asInstanceOf[IR.Pattern.Constructor]
        .constructor
        .getMetadata(Patterns) shouldBe defined
      patterns(1) shouldBe a[IR.Error.Pattern]
      patterns(2)
        .asInstanceOf[IR.Pattern.Constructor]
        .constructor shouldBe a[IR.Error.Resolution]
      patterns(3)
        .asInstanceOf[IR.Pattern.Type]
        .tpe
        .getMetadata(Patterns) shouldBe defined
    }
  }
}

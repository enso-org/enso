package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap.{
  Cons,
  Resolution,
  ResolvedConstructor
}
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.{AliasAnalysis, PatternResolution}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class PatternResolutionTest extends CompilerTest {

  // === Test Setup ===========================================================

  val modCtx: ModuleContext = buildModuleContext(
    freshNameSupply = Some(new FreshNameSupply)
  )

  val passes = new Passes

  val precursorPasses: PassGroup =
    passes.getPrecursors(PatternResolution).get

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
      PatternResolution.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Pattern resolution" should {
    implicit val ctx: ModuleContext = modCtx

    val ir =
      """
        |type Foo a b c
        | 
        |main = case this of
        |    Foo a b c -> a + b + c
        |    Foo a b -> a + b
        |    Does_Not_Exist x y z -> x + y + z
        |
        |""".stripMargin.preprocessModule.analyse

    "attach resolved atoms to the method definitions" in {
      val patterns = ir
        .bindings(1)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]
        .branches
        .map(_.pattern)
      patterns(0)
        .asInstanceOf[IR.Pattern.Constructor]
        .constructor
        .getMetadata(PatternResolution) shouldEqual Some(
        Resolution(ResolvedConstructor(modCtx.module, Cons("Foo", 3)))
      )
      patterns(1) shouldBe a[IR.Error.Pattern]
      patterns(2)
        .asInstanceOf[IR.Pattern.Constructor]
        .constructor shouldBe a[IR.Error.Resolution]
    }
  }
}

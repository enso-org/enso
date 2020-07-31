package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.Cons
import org.enso.compiler.pass.analyse.BindingResolution

import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class BindingResolutionTest extends CompilerTest {

  // === Test Setup ===========================================================

  val modCtx: ModuleContext = buildModuleContext(
    freshNameSupply = Some(new FreshNameSupply)
  )

  val passes = new Passes

  val precursorPasses: PassGroup = passes.getPrecursors(BindingResolution).get

  val passConfiguration: PassConfiguration = PassConfiguration()

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
      BindingResolution.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Tail call analysis on modules" should {
    implicit val ctx: ModuleContext = modCtx

    val ir =
      """
        |type Foo a b c
        |type Bar
        |type Baz x y
        | 
        |Baz.foo = 123
        |Bar.baz = Baz 1 2 . foo
        |""".stripMargin.preprocessModule.analyse

    "discover all atoms in a module" in {
      ir.getMetadata(BindingResolution) shouldEqual Some(
        BindingsMap(
          List(Cons("Foo", 3), Cons("Bar", 0), Cons("Baz", 2)),
          modCtx.module
        )
      )
    }
  }
}

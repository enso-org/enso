package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.{AliasAnalysis, UndefinedVariables}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class UndefinedVariablesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  /** The passes that must be run before the demand analysis pass. */
  val precursorPasses: PassGroup = passes.getPrecursors(UndefinedVariables).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis         -->> AliasAnalysis.Configuration(),
    ApplicationSaturation -->> ApplicationSaturation.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run alias analysis on an [[IR.Module]].
    *
    * @param ir the module to run alias analysis on
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Runs demand analysis on a module.
      *
      * @return [[ir]], transformed by the demand analysis pass
      */
    def analyse: IR.Module = {
      UndefinedVariables.runModule(ir, buildModuleContext())
    }
  }

  def mkModuleContext: ModuleContext = {
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )
  }

  // === The Tests ============================================================

  "Undefined variables" should {
    "be detected and reported" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |my_func (fn = foobar) (w = fn) =
          |    x = [1, 2, y]
          |    x.map (+ 5)
          |    x.fold .to_json
          |    x + z
          |""".stripMargin.preprocessModule.analyse
      val unresolved = ir.preorder.collect {
        case IR.Error.Resolution(
              name,
              IR.Error.Resolution.VariableNotInScope,
              _,
              _
            ) =>
          name
      }
      unresolved.map(_.name) shouldEqual List("foobar", "y", "z")
    }
  }
}

package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration.ToPair
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.resolve.VectorLiterals
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class VectorLiteralsTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes

  val precursorPasses: PassGroup =
    passes.getPrecursors(VectorLiterals).get

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
      VectorLiterals.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Pattern resolution" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |foo (x = [1, 2, 3]) =
        |     foo = ["foo", bar, [1,2,3]]
        |     x
        |
        |""".stripMargin.preprocessModule.analyse

    "transform vector literals into applications" in {
      val fun = ir
        .bindings(0)
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
        .body
        .asInstanceOf[IR.Function.Lambda]

      val arg = fun.arguments(1).defaultValue.get
      arg shouldBe an[IR.Application.Prefix]
      arg
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
        .value shouldBe an[IR.Application.Literal.Sequence]

      val bodyLiteral = fun.body
        .asInstanceOf[IR.Expression.Block]
        .expressions(0)
        .asInstanceOf[IR.Expression.Binding]
        .expression

      bodyLiteral shouldBe an[IR.Application.Prefix]

      val outerVec =
        bodyLiteral.asInstanceOf[IR.Application.Prefix].arguments(0).value

      outerVec shouldBe an[IR.Application.Literal.Sequence]

      val innerLiteral =
        outerVec.asInstanceOf[IR.Application.Literal.Sequence].items(2)

      innerLiteral shouldBe an[IR.Application.Prefix]

      innerLiteral
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
        .value shouldBe an[IR.Application.Literal.Sequence]
    }
  }
}

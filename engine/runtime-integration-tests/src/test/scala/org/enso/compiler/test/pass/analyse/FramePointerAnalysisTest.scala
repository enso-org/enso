package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Module
import org.enso.compiler.pass.analyse.alias.Info
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.analyse.{AliasAnalysis, FramePointerAnalysis}
import org.enso.compiler.test.CompilerTest


class FramePointerAnalysisTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(FramePointerAnalysis).get

  val passConfiguration: PassConfiguration = PassConfiguration()

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
    def analyse(implicit context: ModuleContext): Module = {
      FramePointerAnalysis.runModule(ir, context)
    }
  }

  // === The Tests ============================================================
  "Frame pointer analysis" should {
    "attach frame pointers to a simple method" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main =
          |    a = 1
          |    b = 2
          |    42
          |""".stripMargin.preprocessModule.analyse
      val allOcc = collectAllOccurences(ir.bindings.head)
      allOcc.size shouldBe 2
      val firstOcc = allOcc.head
      firstOcc._1
        .asInstanceOf[Expression.Binding]
        .name
        .name shouldEqual "a"
      val secondOcc = allOcc.last
      secondOcc._1
        .asInstanceOf[Expression.Binding]
        .name
        .name shouldEqual "b"
    }
  }

  private def collectAllOccurences(
    ir: IR
  ): List[(IR, Info.Occurrence)] = {
    ir.preorder().flatMap { childIr =>
      childIr.getMetadata(AliasAnalysis) match {
        case Some(occMeta: Info.Occurrence) =>
          Some((childIr, occMeta))
        case _ => None
      }
    }
  }
}

package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FramePointer, FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Module
import org.enso.compiler.pass.analyse.alias.{Graph, Info}
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
    "attach frame pointers to local variables" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main =
          |    a = 1
          |    b = 2
          |    42
          |""".stripMargin.preprocessModule.analyse
      val allOcc = collectAllOccurences(ir)
      allOcc.size shouldBe 2
      withClue("Occurences are attached to Expression.Binding") {
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
      val framePointers = collectAllFramePointers(ir)
      withClue(
        "There should be the exact same amount of AliasAnalysis Uses and FramePointers metadata"
      ) {
        allOcc.size shouldEqual framePointers.size
      }
      framePointers.head._2.framePointer shouldEqual new FramePointer(0, 1)
      framePointers.last._2.framePointer shouldEqual new FramePointer(0, 2)
    }

    "attach frame pointers to parameters" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main x y = x + y
          |""".stripMargin.preprocessModule.analyse
      val framePointers = collectAllFramePointers(ir)
      framePointers.head._2.framePointer shouldEqual new FramePointer(0, 1)
      framePointers.last._2.framePointer shouldEqual new FramePointer(0, 2)
    }

    "attach frame pointers inside nested scope" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main =
          |    nested x y = x + y
          |    nested 1 2
          |""".stripMargin.preprocessModule.analyse
      val mainScope = ir.bindings.head
        .unsafeGetMetadata(AliasAnalysis, "should exist")
        .asInstanceOf[Info.Scope.Root]
      val xUseId = mainScope.graph.symbolToIds[Graph.Occurrence.Use]("x").head
      val yUseId = mainScope.graph.symbolToIds[Graph.Occurrence.Use]("y").head
      val nestedUseId =
        mainScope.graph.symbolToIds[Graph.Occurrence.Use]("nested").head
      val allOccurences = collectAllOccurences(ir)
      val xIr = allOccurences
        .find { case (_, occ) => occ.id == xUseId }
        .map(_._1)
        .get
      val yIr = allOccurences
        .find { case (_, occ) => occ.id == yUseId }
        .map(_._1)
        .get
      val nestedIr = allOccurences
        .find { case (_, occ) => occ.id == nestedUseId }
        .map(_._1)
        .get
      withClue("All Uses must have FramePointerMeta associated") {
        xIr.passData().get(FramePointerAnalysis) shouldBe defined
        yIr.passData().get(FramePointerAnalysis) shouldBe defined
        nestedIr.passData().get(FramePointerAnalysis) shouldBe defined
      }
      xIr
        .unsafeGetMetadata(FramePointerAnalysis, "should exist")
        .framePointer shouldEqual new FramePointer(0, 1)
      yIr
        .unsafeGetMetadata(FramePointerAnalysis, "should exist")
        .framePointer shouldEqual new FramePointer(0, 2)
      nestedIr
        .unsafeGetMetadata(FramePointerAnalysis, "should exist")
        .framePointer shouldEqual new FramePointer(0, 1)
    }

    "attach frame pointer in nested scope that uses parent scope" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main =
          |    x = 1
          |    nested =
          |        x + 1
          |    nested
          |""".stripMargin.preprocessModule.analyse
      val mainScope = ir.bindings.head
        .unsafeGetMetadata(AliasAnalysis, "should exist")
        .asInstanceOf[Info.Scope.Root]
      val xUseId        = mainScope.graph.symbolToIds[Graph.Occurrence.Use]("x").head
      val allOccurences = collectAllOccurences(ir)
      val xIr = allOccurences
        .find { case (_, occ) => occ.id == xUseId }
        .map(_._1)
        .get
      xIr
        .unsafeGetMetadata(FramePointerAnalysis, "should exist")
        .framePointer shouldEqual new FramePointer(1, 1)
      val nestedUseId =
        mainScope.graph.symbolToIds[Graph.Occurrence.Use]("nested").head
      val nestedIr = allOccurences
        .find { case (_, occ) => occ.id == nestedUseId }
        .map(_._1)
        .get
      nestedIr
        .unsafeGetMetadata(FramePointerAnalysis, "should exist")
        .framePointer shouldEqual new FramePointer(0, 2)
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

  private def collectAllFramePointers(
    ir: IR
  ): List[(IR, FramePointerAnalysis.Metadata)] = {
    ir.preorder().flatMap { childIr =>
      childIr.getMetadata(FramePointerAnalysis) match {
        case Some(framePointerMeta: FramePointerAnalysis.Metadata) =>
          Some((childIr, framePointerMeta))
        case _ => None
      }
    }
  }
}

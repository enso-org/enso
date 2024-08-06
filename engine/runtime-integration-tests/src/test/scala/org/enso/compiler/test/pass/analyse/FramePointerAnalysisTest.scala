package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FramePointer, FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{DefinitionArgument, Expression, Module}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.pass.analyse.alias.{Graph, Info}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.analyse.{AliasAnalysis, FramePointerAnalysis}
import org.enso.compiler.test.CompilerTest

import scala.reflect.ClassTag

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
      withClue("Expression.Binding must have FramePointer associated") {
        allOcc.head._1
          .unsafeGetMetadata(FramePointerAnalysis, "should exist")
          .framePointer shouldEqual new FramePointer(0, 1)
        allOcc.last._1
          .unsafeGetMetadata(FramePointerAnalysis, "should exist")
          .framePointer shouldEqual new FramePointer(0, 2)
      }
    }

    "attach frame pointers to parameters" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |main x y = x + y
          |""".stripMargin.preprocessModule.analyse
      val aliasGraph = ir.bindings.head
        .unsafeGetMetadata(AliasAnalysis, "should exist")
        .asInstanceOf[Info.Scope.Root]
        .graph
      val xDefIr = findAssociatedIr(
        aliasGraph.symbolToIds[Graph.Occurrence.Def]("x").head,
        ir
      )
      expectFramePointer(xDefIr, new FramePointer(0, 1))

      val xUseIr = findAssociatedIr(
        aliasGraph.symbolToIds[Graph.Occurrence.Use]("x").head,
        ir
      )
      withClue(
        "x is used as an argument to a method call, so is in a separate scope from root scope." +
        " Its FramePointer should therefore have parentLevel = 1"
      ) {
        expectFramePointer(xUseIr, new FramePointer(1, 1))
      }

      val plusUseIr = findAssociatedIr(
        aliasGraph.symbolToIds[Graph.Occurrence.Use]("+").head,
        ir
      )
      withClue(
        "There should be no associated FramePointer with usage of `+`, because it is not defined " +
        "in any scope"
      ) {
        plusUseIr.passData().get(FramePointerAnalysis) shouldNot be(defined)
      }
      val framePointers = collectAllFramePointers(ir)
      framePointers.size shouldBe 4
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
      val allFps = collectAllFramePointers(ir)
      allFps.size shouldBe 4
      val xDefIr = findAssociatedIr(
        mainScope.graph.symbolToIds[Graph.Occurrence.Def]("x").head,
        ir
      )
      expectFramePointer(xDefIr, new FramePointer(0, 1))

      val nestedDefIr = findAssociatedIr(
        mainScope.graph.symbolToIds[Graph.Occurrence.Def]("nested").head,
        ir
      )
      expectFramePointer(nestedDefIr, new FramePointer(0, 2))

      val xUseIr = findAssociatedIr(
        mainScope.graph.symbolToIds[Graph.Occurrence.Use]("x").head,
        ir
      )
      expectFramePointer(xUseIr, new FramePointer(2, 1))

      val nestedUseIr = findAssociatedIr(
        mainScope.graph.symbolToIds[Graph.Occurrence.Use]("nested").head,
        ir
      )
      expectFramePointer(nestedUseIr, new FramePointer(0, 2))
    }

    "attach frame pointers to constructor parameters" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    Cons x y
          |""".stripMargin.preprocessModule.analyse
      val xArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "x")
      val yArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "y")
      expectFramePointer(xArg, new FramePointer(0, 1))
      expectFramePointer(yArg, new FramePointer(0, 2))
    }

    "attach frame pointers to constructor parameters with default values" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    Cons x=1 y=(x + 1)
          |""".stripMargin.preprocessModule.analyse
      val xArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "x")
      val yArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "y")
      expectFramePointer(xArg, new FramePointer(0, 1))
      expectFramePointer(yArg, new FramePointer(0, 2))
    }

    "attach frame pointers to constructor parameters with type ascriptions" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Integer
          |type My_Type
          |    Cons x:My_Integer (y:My_Integer = 1)
          |""".stripMargin.preprocessModule.analyse
      val xArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "x")
      val yArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "y")
      expectFramePointer(xArg, new FramePointer(0, 1))
      expectFramePointer(yArg, new FramePointer(0, 2))
    }
  }

  /** Find the first IR element of the given `T` type by the given `filterCondition`.
    * @param filterCondition Filter condition will be applied to all the elements of the desired type.
    *                        The first element that matches the condition will be returned
    * @tparam T type of the IR element to be found
    * @return
    */
  private def findIRElement[T <: IR: ClassTag](
    rootIr: IR,
    filterCondition: T => Boolean
  ): T = {
    rootIr
      .preorder()
      .flatMap {
        case childIr: T =>
          Some(childIr).filter(filterCondition)
        case _ => None
      }
      .head
  }

  /** Asserts that the given `ir` has the given `framePointer` attached as metadata.
    */
  private def expectFramePointer(
    ir: IR,
    framePointer: FramePointer
  ): Unit = {
    withClue("FramePointerAnalysis metadata should be attached to the IR") {
      ir.passData().get(FramePointerAnalysis) shouldBe defined
    }
    ir
      .unsafeGetMetadata(FramePointerAnalysis, "should exist")
      .framePointer shouldEqual framePointer
  }

  private def findAssociatedIr(
    id: Graph.Id,
    moduleIr: IR
  ): IR = {
    val irs = moduleIr.preorder().collect { childIr =>
      childIr.getMetadata(AliasAnalysis) match {
        case Some(Info.Occurrence(_, occId)) if occId == id =>
          childIr
      }
    }
    withClue(
      "There should be just one IR element that has a particular Graph.ID"
    ) {
      irs.size shouldBe 1
    }
    irs.head
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

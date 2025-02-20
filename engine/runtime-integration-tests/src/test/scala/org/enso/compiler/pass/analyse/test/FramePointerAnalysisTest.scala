package org.enso.compiler.pass.analyse.test

import org.enso.compiler.Passes
import org.enso.compiler.pass.analyse.FramePointer
import org.enso.compiler.pass.analyse.FrameAnalysisMeta
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Module,
  Name
}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.pass.analyse.alias.graph.{Graph, GraphOccurrence}
import org.enso.compiler.pass.analyse.alias.AliasMetadata
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.analyse.{
  alias,
  AliasAnalysis,
  FramePointerAnalysis
}
import org.enso.compiler.pass.resolve.GlobalNames
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
          .asInstanceOf[FramePointer] shouldEqual new FramePointer(0, 1)
        allOcc.last._1
          .unsafeGetMetadata(FramePointerAnalysis, "should exist")
          .asInstanceOf[FramePointer] shouldEqual new FramePointer(0, 2)
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
        .asInstanceOf[AliasMetadata.RootScope]
        .graph
      val xDefIr = findAssociatedIr(
        aliasGraph.symbolToIds[GraphOccurrence.Def]("x").head,
        ir
      )
      expectFramePointer(xDefIr, new FramePointer(0, 1))

      val xUseIr = findAssociatedIr(
        aliasGraph.symbolToIds[GraphOccurrence.Use]("x").head,
        ir
      )
      withClue(
        "x is used as an argument to a method call, so is in a separate scope from root scope." +
        " Its FramePointer should therefore have parentLevel = 1"
      ) {
        expectFramePointer(xUseIr, new FramePointer(1, 1))
      }

      val plusUseIr = findAssociatedIr(
        aliasGraph.symbolToIds[GraphOccurrence.Use]("+").head,
        ir
      )
      withClue(
        "There should be no associated FramePointer with usage of `+`, because it is not defined " +
        "in any scope"
      ) {
        findFP(plusUseIr) shouldNot be(defined)
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
        .asInstanceOf[AliasMetadata.RootScope]
      val allFps = collectAllFramePointers(ir)
      allFps.size shouldBe 4
      val xDefIr = findAssociatedIr(
        mainScope.graph.symbolToIds[GraphOccurrence.Def]("x").head,
        ir
      )
      expectFramePointer(xDefIr, new FramePointer(0, 1))

      val nestedDefIr = findAssociatedIr(
        mainScope.graph.symbolToIds[GraphOccurrence.Def]("nested").head,
        ir
      )
      expectFramePointer(nestedDefIr, new FramePointer(0, 2))

      val xUseIr = findAssociatedIr(
        mainScope.graph.symbolToIds[GraphOccurrence.Use]("x").head,
        ir
      )
      expectFramePointer(xUseIr, new FramePointer(2, 1))

      val nestedUseIr = findAssociatedIr(
        mainScope.graph.symbolToIds[GraphOccurrence.Use]("nested").head,
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

    "attach frame pointers to synthetic self argument" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    static_method = 42
          |""".stripMargin.preprocessModule.analyse
      // synthetic self argument does not have location and is `synthetic == true`.
      val syntheticSelfArg = findIRElement[DefinitionArgument.Specified](
        ir,
        _.name match {
          case Name.Self(loc, synthetic, _) if loc == null && synthetic =>
            true
          case _ => false
        }
      )
      expectFramePointer(syntheticSelfArg, new FramePointer(0, 1))
    }

    "attach frame pointers to arguments with synthetic self" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    static_method x = 42
          |""".stripMargin.preprocessModule.analyse
      // synthetic self argument does not have location and is `synthetic == true`.
      val syntheticSelfArg = findIRElement[DefinitionArgument.Specified](
        ir,
        _.name match {
          case Name.Self(loc, synthetic, _) if loc == null && synthetic =>
            true
          case _ => false
        }
      )
      val xArg =
        findIRElement[DefinitionArgument.Specified](ir, _.name.name == "x")
      expectFramePointer(syntheticSelfArg, new FramePointer(0, 1))
      expectFramePointer(xArg, new FramePointer(0, 2))
    }

    "attach frame pointers to self argument" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |    instance_method self = self.data + 42
          |""".stripMargin.preprocessModule.analyse
      val selfArg = findIRElement[DefinitionArgument.Specified](
        ir,
        _.name.isInstanceOf[Name.Self]
      )
      expectFramePointer(selfArg, new FramePointer(0, 1))
    }

    "attach frame pointers to internal variables" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |
          |method x =
          |    case x of
          |        _ : My_Type -> 42
          |        _ -> 0
          |""".stripMargin.preprocessModule.analyse
      // <internal-0> = (FORCE x)
      // case (FORCE <internal-0>) of
      //   <internal-1> : My_Type -> 42
      //   <internal-2> -> 0
      val allOcc = collectAllOccurences(ir)
      val internal0 =
        findIRElement[Expression.Binding](ir, _.name.name == "<internal-0>")
      val internal1 = findIRElement[Name.Literal](ir, _.name == "<internal-1>")
      val internal2 = findIRElement[Name.Literal](ir, _.name == "<internal-2>")
      // internal1 and internal2 are in different scopes.
      withClue(
        "<internal-0> is assigned after x is assigned and x has FramePointer(0, 2)"
      ) {
        expectFramePointer(internal0, new FramePointer(0, 3))
      }
      withClue("<internal-1> and <internal-2> are in different scopes") {
        expectFramePointer(internal1, new FramePointer(0, 1))
        expectFramePointer(internal2, new FramePointer(0, 1))
      }
      allOcc shouldNot be(null)
    }

    "attach frame pointers to arguments in annotation" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type List
          |    @index (t-> t + 1)
          |    at self index =
          |        42
          |""".stripMargin.preprocessModule.analyse
      val tDefArg = findIRElement[DefinitionArgument.Specified](
        ir,
        arg => arg.name.name == "t"
      )
      val tUseArg = findIRElement[CallArgument.Specified](
        ir,
        arg => {
          arg.value match {
            case lit: Name.Literal =>
              lit.name == "t"
            case _ => false
          }
        }
      )
      expectFramePointer(tDefArg, new FramePointer(0, 1))
      expectFramePointer(tUseArg.value, new FramePointer(1, 1))
    }

    "attach frame pointers to argument default value expression (1)" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |method fn=(\x -> x + 1) =
          |    fn 42
          |""".stripMargin.preprocessModule.analyse
      val xDefArg = findIRElement[DefinitionArgument.Specified](
        ir,
        arg => arg.name.name == "x"
      )
      val xUseArg = findIRElement[CallArgument.Specified](
        ir,
        arg => {
          arg.value match {
            case lit: Name.Literal =>
              lit.name == "x"
            case _ => false
          }
        }
      )
      expectFramePointer(xDefArg, new FramePointer(0, 1))
      expectFramePointer(xUseArg.value, new FramePointer(1, 1))
    }

    "attach frame pointer to argument default value expression using previous argument" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |method x y=(x + 1) =
          |    42
          |""".stripMargin.preprocessModule.analyse
      val allOcc = collectAllOccurences(ir)
      val fps    = collectAllFramePointers(ir)
      // `xLit` is the literal used in the `(x + 1)` expression
      val xLit = findIRElements[Name.Literal](
        ir,
        lit => {
          lit.name == "x"
        }
      ).last
      expectFramePointer(xLit, new FramePointer(1, 2))
      allOcc shouldNot be(null)
      fps shouldNot be(null)
    }

    "attach frame pointer to argument default value expression (2)" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |find data_link_instance (if_not_supported = (Error.throw (Illegal_Argument.Error "The "+(data_link_name data_link_instance)+" cannot be opened as a stream."))) =
          |    42
          |""".stripMargin.preprocessModule.analyse
      // The literal used at the end of the line
      val dataLinkInstanceLit = findIRElements[Name.Literal](
        ir,
        lit => {
          lit.name == "data_link_instance"
        }
      ).last
      expectFramePointer(dataLinkInstanceLit, new FramePointer(5, 2))
    }

    "does not attach frame pointer to argument default value literal" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |method lit="Some Text" =
          |    lit
          |""".stripMargin.preprocessModule.analyse
      val litDefArg = findIRElement[DefinitionArgument.Specified](
        ir,
        arg => arg.name.name == "lit"
      )
      litDefArg.defaultValue.get
        .passData()
        .get(FramePointerAnalysis) shouldNot be(defined)
    }

    "does not attach frame pointer to name literal that is resolved to global type in the same module" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |method =
          |    My_Type
          |""".stripMargin.preprocessModule.analyse
      // `My_TYpe` literal used in the method body
      val myTypeLit = findIRElements[Name.Literal](
        ir,
        lit => lit.name == "My_Type"
      ).last
      withClue("No frame pointer attached to a symbol with global occurence") {
        findFP(myTypeLit) shouldNot be(defined)
      }
      withClue("There is a Use occurence") {
        myTypeLit.passData.get(AliasAnalysis) shouldBe defined
      }
      withClue("There is Resolution attached") {
        myTypeLit.passData.get(GlobalNames) shouldBe defined
      }
    }

    "does not attach frame pointer to name literal in annotation that is resolved to global type in the same module" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |
          |@x My_Type
          |method x =
          |    My_Type
          |""".stripMargin.preprocessModule.analyse
      // literal used in the annotation
      val myTypeLit = findIRElements[Name.Literal](
        ir,
        lit => lit.name == "My_Type"
      ).apply(1)
      withClue("No frame pointer attached to a symbol with global occurence") {
        findFP(myTypeLit) shouldNot be(defined)
      }
      withClue("There is a Use occurence") {
        myTypeLit.passData.get(AliasAnalysis) shouldBe defined
      }
      withClue("There is Resolution attached") {
        myTypeLit.passData.get(GlobalNames) shouldBe defined
      }
    }
  }

  private def findFP(ir: IR) = ir.passData
    .get(FramePointerAnalysis)
    .filter(meta => meta.isInstanceOf[FramePointer])

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

  private def findIRElements[T <: IR: ClassTag](
    rootIr: IR,
    filterCondition: T => Boolean
  ): List[T] = {
    rootIr
      .preorder()
      .flatMap {
        case childIr: T =>
          Some(childIr).filter(filterCondition)
        case _ => None
      }
      .toList
  }

  /** Asserts that the given `ir` has the given `framePointer` attached as metadata.
    */
  private def expectFramePointer(
    ir: IR,
    framePointer: FramePointer
  ): Unit = {
    withClue(
      "FramePointerAnalysis metadata should be attached to the IR " + ir
    ) {
      ir.passData().get(FramePointerAnalysis) shouldBe defined
    }
    ir
      .unsafeGetMetadata(FramePointerAnalysis, "should exist")
      .asInstanceOf[FramePointer] shouldEqual framePointer
  }

  private def findAssociatedIr(
    id: Graph.Id,
    moduleIr: IR
  ): IR = {
    val irs = moduleIr.preorder().collect { childIr =>
      childIr.getMetadata(AliasAnalysis) match {
        case Some(AliasMetadata.Occurrence(_, occId)) if occId == id =>
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
  ): List[(IR, AliasMetadata.Occurrence)] = {
    ir.preorder().flatMap { childIr =>
      childIr.getMetadata(AliasAnalysis) match {
        case Some(occMeta: alias.AliasMetadata.Occurrence) =>
          Some((childIr, occMeta))
        case _ => None
      }
    }
  }

  private def collectAllFramePointers(
    ir: IR
  ): List[(IR, FrameAnalysisMeta)] = {
    ir.preorder().flatMap { childIr =>
      childIr.getMetadata(FramePointerAnalysis) match {
        case Some(framePointerMeta: FrameAnalysisMeta)
            if (framePointerMeta.isInstanceOf[FramePointer]) =>
          Some((childIr, framePointerMeta))
        case _ => None
      }
    }
  }
}

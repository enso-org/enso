package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Pattern
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo.Type.asStatic
import org.enso.compiler.pass.analyse.{AliasAnalysis, DataflowAnalysis}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.interpreter.test.Metadata
import org.scalatest.Assertion

class DataflowAnalysisTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  /** The passes that must be run before the dataflow analysis pass. */
  val precursorPasses: PassGroup =
    passes.getPrecursors(DataflowAnalysis).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis         -->> AliasAnalysis.Configuration(),
    ApplicationSaturation -->> ApplicationSaturation.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Generates an identifier dependency.
    *
    * @return a randomly generated identifier dependency
    */
  def genStaticDep: DependencyInfo.Type = {
    DependencyInfo.Type.Static(genId, None)
  }

  /** Makes a statically known dependency from the included id.
    *
    * @param id the identifier to use as the id
    * @return a static dependency on the node given by `id`
    */
  def mkStaticDep(id: DependencyInfo.Identifier): DependencyInfo.Type = {
    mkStaticDep(id, None)
  }

  /** Makes a statically known dependency from the provided identifiers.
    *
    * @param id the internal id
    * @param extId the external id of the node
    * @return a static dependency on the node corresponding to `ir`, `extId`
    */
  def mkStaticDep(
    id: DependencyInfo.Identifier,
    extId: Option[IR.ExternalId]
  ): DependencyInfo.Type = {
    DependencyInfo.Type.Static(id, extId)
  }

  /** Makes a symbol dependency from the included string.
    *
    * @param str the string to use as a name
    * @return a symbol dependency on the symbol given by `str`
    */
  def mkDynamicDep(str: String): DependencyInfo.Type = {
    mkDynamicDep(str, None)
  }

  /** Makes a symbol dependency from the included string and identifier.
    *
    * @param str the string to use as a name
    * @param extId the external identifier corresponding to `str`
    * @return a symbol dependency on the symbol given by `str`
    */
  def mkDynamicDep(
    str: String,
    extId: Option[IR.Identifier]
  ): DependencyInfo.Type = {
    DependencyInfo.Type.Dynamic(str, extId)
  }

  /** Adds an extension method to run dataflow analysis on an [[IR.Module]].
    *
    * @param ir the module to run dataflow analysis on.
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Runs dataflow analysis on a module.
      *
      * @return [[ir]], with attached data dependency information
      */
    def analyse: IR.Module = {
      DataflowAnalysis.runModule(
        ir,
        buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
      )
    }
  }

  /** Adds an extension method to run dataflow analysis on an [[IR.Expression]].
    *
    * @param ir the expression to run dataflow analysis on
    */
  implicit class AnalyseExpresion(ir: IR.Expression) {

    /** Runs dataflow analysis on an expression.
      *
      * @param inlineContext the inline context in which to process the
      *                      expression
      * @return [[ir]], with attached data dependency information
      */
    def analyse(implicit inlineContext: InlineContext): IR.Expression = {
      DataflowAnalysis.runExpression(ir, inlineContext)
    }
  }

  /** Adds an extension method to check whether the target IR node has
    * associated dataflow analysis metadata.
    *
    * @param ir the IR node to check
    */
  implicit class HasDependencyInfo(ir: IR) {

    /** Checks if [[ir]] has associated [[DataflowAnalysis.Metadata]].
      *
      * @return `true` if [[ir]] has the associated metadata, otherwise `false`
      */
    def hasDependencyInfo: Assertion = {
      ir.getMetadata(DataflowAnalysis) shouldBe defined
    }
  }

  /** Generates a new inline context for testing purposes.
    *
    * @return a new inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(
      localScope       = Some(LocalScope.root),
      isInTailPosition = Some(false),
      freshNameSupply  = Some(new FreshNameSupply)
    )
  }

  /** Generates a new module context for testing purposes.
    *
    * @return a new module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )
  }

  // === The Tests ============================================================

  "Dataflow metadata" should {
    "allow querying for expressions that should be invalidated on change" in {
      val dependencies = new DependencyInfo
      val ids          = List.fill(5)(genStaticDep)

      dependencies(ids.head) = Set(ids(1), ids(2))
      dependencies(ids(2))   = Set(ids(3), ids(4))
      dependencies(ids(4))   = Set(ids(1), ids.head)

      dependencies(ids.head) shouldEqual Set(
        ids(1),
        ids(2),
        ids(3),
        ids(4),
        ids.head
      )
    }

    "provide a safe query function as well" in {
      val dependencies = new DependencyInfo
      val ids          = List.fill(5)(genStaticDep)
      val badId        = genStaticDep

      dependencies(ids.head) = Set(ids(1), ids(2))
      dependencies(ids(2))   = Set(ids(3), ids(4))
      dependencies(ids(4))   = Set(ids(1), ids.head)

      dependencies.get(ids.head) shouldBe defined
      dependencies.get(badId) should not be defined

      dependencies.get(ids.head) shouldEqual Some(
        Set(
          ids(1),
          ids(2),
          ids(3),
          ids(4),
          ids.head
        )
      )
    }

    "allow querying only the direct dependents of a node" in {
      val dependencies = new DependencyInfo
      val ids          = List.fill(5)(genStaticDep)

      dependencies(ids.head) = Set(ids(1), ids(2))
      dependencies(ids(2))   = Set(ids(3), ids(4))
      dependencies(ids(4))   = Set(ids(1), ids.head)

      dependencies.getDirect(ids.head) shouldEqual Some(Set(ids(1), ids(2)))
      dependencies.getDirect(ids(2)) shouldEqual Some(Set(ids(3), ids(4)))
      dependencies.getDirect(ids(4)) shouldEqual Some(Set(ids(1), ids.head))
    }

    "allow for updating the dependents of a node" in {
      val dependencies = new DependencyInfo
      val ids          = List.fill(3)(genStaticDep)

      dependencies(ids.head) = Set(ids(1))
      dependencies(ids.head) shouldEqual Set(ids(1))

      dependencies(ids.head) = Set(ids(2))
      dependencies(ids.head) shouldEqual Set(ids(2))

      dependencies(ids.head) ++= Set(ids(1))
      dependencies(ids.head) shouldEqual Set(ids(1), ids(2))
    }

    "allow for updating at a given node" in {
      val dependencies = new DependencyInfo
      val ids          = List.fill(6)(genStaticDep)
      val set1         = Set.from(ids.tail)
      val newId        = genStaticDep

      dependencies.updateAt(ids.head, set1)
      dependencies(ids.head) shouldEqual set1

      dependencies.updateAt(ids.head, Set(newId))
      dependencies(ids.head) shouldEqual (set1 + newId)
    }

    "allow combining the information from multiple modules" in {
      val module1 = new DependencyInfo
      val module2 = new DependencyInfo

      val symbol1 = mkDynamicDep("foo")
      val symbol2 = mkDynamicDep("bar")
      val symbol3 = mkDynamicDep("baz")

      val symbol1DependentIdsInModule1 = Set(genStaticDep, genStaticDep)
      val symbol2DependentIdsInModule1 = Set(genStaticDep, genStaticDep)
      val symbol1DependentIdsInModule2 = Set(genStaticDep, genStaticDep)
      val symbol3DependentIdsInModule2 = Set(genStaticDep)

      module1(symbol1) = symbol1DependentIdsInModule1
      module1(symbol2) = symbol2DependentIdsInModule1
      module2(symbol1) = symbol1DependentIdsInModule2
      module2(symbol3) = symbol3DependentIdsInModule2

      val combinedModule = module1 ++ module2

      combinedModule.get(symbol1) shouldBe defined
      combinedModule.get(symbol2) shouldBe defined
      combinedModule.get(symbol3) shouldBe defined

      val symbol1DependentIdsCombined =
        symbol1DependentIdsInModule1 ++ symbol1DependentIdsInModule2

      combinedModule(symbol1) shouldEqual symbol1DependentIdsCombined
      combinedModule(symbol2) shouldEqual symbol2DependentIdsInModule1
      combinedModule(symbol3) shouldEqual symbol3DependentIdsInModule2
    }
  }

  "Whole-module dataflow analysis" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |M.foo = a -> b ->
        |    IO.println b
        |    c = a + b
        |    frobnicate a c
        |""".stripMargin.preprocessModule.analyse

    val depInfo = ir.getMetadata(DataflowAnalysis).get

    // The method and body
    val method =
      ir.bindings.head.asInstanceOf[IR.Module.Scope.Definition.Method]
    val fn = method.body.asInstanceOf[IR.Function.Lambda]
    val fnArgThis =
      fn.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
    val fnArgA = fn.arguments(1).asInstanceOf[IR.DefinitionArgument.Specified]
    val fnArgB = fn.arguments(2).asInstanceOf[IR.DefinitionArgument.Specified]
    val fnBody = fn.body.asInstanceOf[IR.Expression.Block]

    // The `IO.println` expression
    val printlnExpr =
      fnBody.expressions.head.asInstanceOf[IR.Application.Prefix]
    val printlnFn = printlnExpr.function.asInstanceOf[IR.Name.Literal]
    val printlnArgIO =
      printlnExpr.arguments.head.asInstanceOf[IR.CallArgument.Specified]
    val printlnArgIOExpr = printlnArgIO.value.asInstanceOf[IR.Error.Resolution]
    val printlnArgB =
      printlnExpr.arguments(1).asInstanceOf[IR.CallArgument.Specified]
    val printlnArgBExpr = printlnArgB.value.asInstanceOf[IR.Name.Literal]

    // The `c =` expression
    val cBindExpr  = fnBody.expressions(1).asInstanceOf[IR.Expression.Binding]
    val cBindName  = cBindExpr.name.asInstanceOf[IR.Name.Literal]
    val plusExpr   = cBindExpr.expression.asInstanceOf[IR.Application.Prefix]
    val plusExprFn = plusExpr.function.asInstanceOf[IR.Name.Literal]
    val plusExprArgA =
      plusExpr.arguments.head.asInstanceOf[IR.CallArgument.Specified]
    val plusExprArgAExpr = plusExprArgA.value.asInstanceOf[IR.Name.Literal]
    val plusExprArgB =
      plusExpr.arguments(1).asInstanceOf[IR.CallArgument.Specified]
    val plusExprArgBExpr = plusExprArgB.value.asInstanceOf[IR.Name.Literal]

    // The `frobnicate` return expression
    val frobExpr = fnBody.returnValue.asInstanceOf[IR.Application.Prefix]
    val frobFn   = frobExpr.function.asInstanceOf[IR.Error.Resolution]
    val frobArgA =
      frobExpr.arguments.head.asInstanceOf[IR.CallArgument.Specified]
    val frobArgAExpr = frobArgA.value.asInstanceOf[IR.Name.Literal]
    val frobArgC =
      frobExpr.arguments(1).asInstanceOf[IR.CallArgument.Specified]
    val frobArgCExpr = frobArgC.value.asInstanceOf[IR.Name.Literal]

    // The global symbols
    val frobnicateSymbol = mkDynamicDep("frobnicate")
    val ioSymbol         = mkDynamicDep("IO")
    val printlnSymbol    = mkDynamicDep("println")
    val plusSymbol       = mkDynamicDep("+")

    // The Identifiers
    val methodId           = mkStaticDep(method.getId)
    val fnId               = mkStaticDep(fn.getId)
    val fnArgAId           = mkStaticDep(fnArgA.getId)
    val fnArgBId           = mkStaticDep(fnArgB.getId)
    val fnBodyId           = mkStaticDep(fnBody.getId)
    val printlnExprId      = mkStaticDep(printlnExpr.getId)
    val printlnFnId        = mkStaticDep(printlnFn.getId)
    val printlnArgIOId     = mkStaticDep(printlnArgIO.getId)
    val printlnArgIOExprId = mkStaticDep(printlnArgIOExpr.getId)
    val printlnArgBId      = mkStaticDep(printlnArgB.getId)
    val printlnArgBExprId  = mkStaticDep(printlnArgBExpr.getId)
    val cBindExprId        = mkStaticDep(cBindExpr.getId)
    val cBindNameId        = mkStaticDep(cBindName.getId)
    val plusExprId         = mkStaticDep(plusExpr.getId)
    val plusExprFnId       = mkStaticDep(plusExprFn.getId)
    val plusExprArgAId     = mkStaticDep(plusExprArgA.getId)
    val plusExprArgAExprId = mkStaticDep(plusExprArgAExpr.getId)
    val plusExprArgBId     = mkStaticDep(plusExprArgB.getId)
    val plusExprArgBExprId = mkStaticDep(plusExprArgBExpr.getId)
    val frobExprId         = mkStaticDep(frobExpr.getId)
    val frobFnId           = mkStaticDep(frobFn.getId)
    val frobArgAId         = mkStaticDep(frobArgA.getId)
    val frobArgAExprId     = mkStaticDep(frobArgAExpr.getId)
    val frobArgCId         = mkStaticDep(frobArgC.getId)
    val frobArgCExprId     = mkStaticDep(frobArgCExpr.getId)

    "correctly identify global symbol direct dependents" in {
      depInfo.getDirect(frobnicateSymbol) shouldEqual Some(Set(frobFnId))
      depInfo.getDirect(ioSymbol) shouldEqual Some(Set(printlnArgIOExprId))
      depInfo.getDirect(printlnSymbol) shouldEqual Some(Set(printlnFnId))
      depInfo.getDirect(plusSymbol) shouldEqual Some(Set(plusExprFnId))
    }

    "correctly identify global symbol indirect dependents" in {
      depInfo.get(frobnicateSymbol) shouldEqual Some(
        Set(frobFnId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(ioSymbol) shouldEqual Some(
        Set(printlnArgIOExprId, printlnArgIOId, printlnExprId)
      )
      depInfo.get(printlnSymbol) shouldEqual Some(
        Set(printlnFnId, printlnExprId)
      )
      depInfo.get(plusSymbol) shouldEqual Some(
        Set(
          plusExprFnId,
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
    }

    "correctly identify local direct dependents" in {
      depInfo.getDirect(fnId) shouldEqual Some(Set(methodId))
      depInfo.getDirect(fnArgAId) shouldEqual Some(
        Set(plusExprArgAExprId, frobArgAExprId)
      )
      depInfo.getDirect(fnArgBId) shouldEqual Some(
        Set(printlnArgBExprId, plusExprArgBExprId)
      )
      depInfo.getDirect(fnBodyId) shouldEqual Some(Set(fnId))

      // The `IO.println` expression
      depInfo.getDirect(printlnExprId) should not be defined
      depInfo.getDirect(printlnFnId) shouldEqual Some(Set(printlnExprId))
      depInfo.getDirect(printlnArgIOId) shouldEqual Some(Set(printlnExprId))
      depInfo.getDirect(printlnArgIOExprId) shouldEqual Some(
        Set(printlnArgIOId)
      )
      depInfo.getDirect(printlnArgBId) shouldEqual Some(Set(printlnExprId))
      depInfo.getDirect(printlnArgBExprId) shouldEqual Some(
        Set(printlnArgBId)
      )

      // The `c = ` expression
      depInfo.getDirect(cBindExprId) shouldEqual Some(Set(frobArgCExprId))
      depInfo.getDirect(cBindNameId) shouldEqual Some(Set(cBindExprId))
      depInfo.getDirect(plusExprId) shouldEqual Some(Set(cBindExprId))
      depInfo.getDirect(plusExprFnId) shouldEqual Some(Set(plusExprId))
      depInfo.getDirect(plusExprArgAId) shouldEqual Some(Set(plusExprId))
      depInfo.getDirect(plusExprArgAExprId) shouldEqual Some(
        Set(plusExprArgAId)
      )
      depInfo.getDirect(plusExprArgBId) shouldEqual Some(Set(plusExprId))
      depInfo.getDirect(plusExprArgBExprId) shouldEqual Some(
        Set(plusExprArgBId)
      )

      // The `frobnicate` expression
      depInfo.getDirect(frobExprId) shouldEqual Some(Set(fnBodyId))
      depInfo.getDirect(frobFnId) shouldEqual Some(Set(frobExprId))
      depInfo.getDirect(frobArgAId) shouldEqual Some(Set(frobExprId))
      depInfo.getDirect(frobArgAExprId) shouldEqual Some(Set(frobArgAId))
      depInfo.getDirect(frobArgCId) shouldEqual Some(Set(frobExprId))
      depInfo.getDirect(frobArgCExprId) shouldEqual Some(Set(frobArgCId))
    }

    "correctly identify local indirect dependents" in {
      depInfo.get(fnId) shouldEqual Some(Set(methodId))
      depInfo.get(fnArgAId) shouldEqual Some(
        Set(
          plusExprArgAExprId,
          plusExprArgAId,
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobArgAExprId,
          frobArgAId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(fnArgBId) shouldEqual Some(
        Set(
          printlnArgBExprId,
          printlnArgBId,
          printlnExprId,
          plusExprArgBExprId,
          plusExprArgBId,
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(fnBodyId) shouldEqual Some(Set(fnId, methodId))

      // The `IO.println` expression
      depInfo.get(printlnExprId) should not be defined
      depInfo.get(printlnFnId) shouldEqual Some(Set(printlnExprId))
      depInfo.get(printlnArgIOId) shouldEqual Some(Set(printlnExprId))
      depInfo.get(printlnArgIOExprId) shouldEqual Some(
        Set(printlnArgIOId, printlnExprId)
      )
      depInfo.get(printlnArgBId) shouldEqual Some(Set(printlnExprId))
      depInfo.get(printlnArgBExprId) shouldEqual Some(
        Set(printlnArgBId, printlnExprId)
      )

      // The `c = ` expression
      depInfo.get(cBindExprId) shouldEqual Some(
        Set(frobArgCExprId, frobArgCId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(cBindNameId) shouldEqual Some(
        Set(
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(plusExprId) shouldEqual Some(
        Set(
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(plusExprFnId) shouldEqual Some(
        Set(
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(plusExprArgAId) shouldEqual Some(
        Set(
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(plusExprArgAExprId) shouldEqual Some(
        Set(
          plusExprArgAId,
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )

      depInfo.get(plusExprArgBId) shouldEqual Some(
        Set(
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(plusExprArgBExprId) shouldEqual Some(
        Set(
          plusExprArgBId,
          plusExprId,
          cBindExprId,
          frobArgCExprId,
          frobArgCId,
          frobExprId,
          fnBodyId,
          fnId,
          methodId
        )
      )

      // The `frobnicate` expression
      depInfo.get(frobExprId) shouldEqual Some(
        Set(
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.get(frobFnId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(frobArgAId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(frobArgAExprId) shouldEqual Some(
        Set(frobArgAId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(frobArgCId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.get(frobArgCExprId) shouldEqual Some(
        Set(frobArgCId, frobExprId, fnBodyId, fnId, methodId)
      )
    }

    "associate the dependency info with every node in the IR" in {
      ir.hasDependencyInfo
      method.hasDependencyInfo
      fn.hasDependencyInfo
      fnArgThis.hasDependencyInfo
      fnArgA.hasDependencyInfo
      fnArgB.hasDependencyInfo
      fnBody.hasDependencyInfo

      printlnExpr.hasDependencyInfo
      printlnFn.hasDependencyInfo
      printlnArgIO.hasDependencyInfo
      printlnArgIOExpr.hasDependencyInfo
      printlnArgB.hasDependencyInfo
      printlnArgBExpr.hasDependencyInfo

      cBindExpr.hasDependencyInfo
      cBindName.hasDependencyInfo
      plusExpr.hasDependencyInfo
      plusExprFn.hasDependencyInfo
      plusExprArgA.hasDependencyInfo
      plusExprArgAExpr.hasDependencyInfo
      plusExprArgB.hasDependencyInfo
      plusExprArgBExpr.hasDependencyInfo

      frobExpr.hasDependencyInfo
      frobFn.hasDependencyInfo
      frobArgA.hasDependencyInfo
      frobArgAExpr.hasDependencyInfo
      frobArgC.hasDependencyInfo
      frobArgCExpr.hasDependencyInfo
    }
  }

  "Dataflow analysis" should {
    "work properly for functions" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x -> (y=x) -> x + y
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val fn = ir.asInstanceOf[IR.Function.Lambda]
      val fnArgX =
        fn.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      val fnArgY =
        fn.arguments(1).asInstanceOf[IR.DefinitionArgument.Specified]
      val fnArgYDefault = fnArgY.defaultValue.get.asInstanceOf[IR.Name.Literal]
      val fnBody        = fn.body.asInstanceOf[IR.Application.Prefix]
      val plusFn        = fnBody.function.asInstanceOf[IR.Name.Literal]
      val plusArgX =
        fnBody.arguments.head.asInstanceOf[IR.CallArgument.Specified]
      val plusArgXExpr = plusArgX.value.asInstanceOf[IR.Name.Literal]
      val plusArgY =
        fnBody.arguments(1).asInstanceOf[IR.CallArgument.Specified]
      val plusArgYExpr = plusArgY.value.asInstanceOf[IR.Name.Literal]

      // Identifiers
      val fnId            = mkStaticDep(fn.getId)
      val fnArgXId        = mkStaticDep(fnArgX.getId)
      val fnArgYId        = mkStaticDep(fnArgY.getId)
      val fnArgYDefaultId = mkStaticDep(fnArgYDefault.getId)
      val fnBodyId        = mkStaticDep(fnBody.getId)
      val plusFnId        = mkStaticDep(plusFn.getId)
      val plusArgXId      = mkStaticDep(plusArgX.getId)
      val plusArgXExprId  = mkStaticDep(plusArgXExpr.getId)
      val plusArgYId      = mkStaticDep(plusArgY.getId)
      val plusArgYExprId  = mkStaticDep(plusArgYExpr.getId)

      // Dynamic Symbols
      val plusSym = mkDynamicDep("+")

      // The Tests
      depInfo.getDirect(fnId) should not be defined
      depInfo.getDirect(fnArgXId) shouldEqual Some(
        Set(plusArgXExprId, fnArgYDefaultId)
      )
      depInfo.getDirect(fnArgYId) shouldEqual Some(Set(plusArgYExprId))
      depInfo.getDirect(fnArgYDefaultId) shouldEqual Some(Set(fnArgYId, fnId))
      depInfo.getDirect(fnBodyId) shouldEqual Some(Set(fnId))
      depInfo.getDirect(plusSym) shouldEqual Some(Set(plusFnId))
      depInfo.getDirect(plusArgXId) shouldEqual Some(Set(fnBodyId))
      depInfo.getDirect(plusArgXExprId) shouldEqual Some(Set(plusArgXId))
      depInfo.getDirect(plusArgYId) shouldEqual Some(Set(fnBodyId))
      depInfo.getDirect(plusArgYExprId) shouldEqual Some(Set(plusArgYId))
    }

    "work properly for prefix applications" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |foo (a = 10) (x -> x * x)
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val app   = ir.asInstanceOf[IR.Application.Prefix]
      val appFn = app.function.asInstanceOf[IR.Error.Resolution]
      val appArg10 =
        app.arguments.head.asInstanceOf[IR.CallArgument.Specified]
      val appArg10Expr = appArg10.value.asInstanceOf[IR.Literal.Number]
      val appArg10Name = appArg10.name.get.asInstanceOf[IR.Name.Literal]
      val appArgFn =
        app.arguments(1).asInstanceOf[IR.CallArgument.Specified]
      val lam = appArgFn.value.asInstanceOf[IR.Function.Lambda]
      val lamArgX =
        lam.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      val mul   = lam.body.asInstanceOf[IR.Application.Prefix]
      val mulFn = mul.function.asInstanceOf[IR.Name.Literal]
      val mulArg1 =
        mul.arguments.head.asInstanceOf[IR.CallArgument.Specified]
      val mulArg1Expr = mulArg1.value.asInstanceOf[IR.Name.Literal]
      val mulArg2     = mul.arguments(1).asInstanceOf[IR.CallArgument.Specified]
      val mulArg2Expr = mulArg2.value.asInstanceOf[IR.Name.Literal]

      // Identifiers
      val appId          = mkStaticDep(app.getId)
      val appFnId        = mkStaticDep(appFn.getId)
      val appArg10Id     = mkStaticDep(appArg10.getId)
      val appArg10ExprId = mkStaticDep(appArg10Expr.getId)
      val appArg10NameId = mkStaticDep(appArg10Name.getId)
      val appArgFnId     = mkStaticDep(appArgFn.getId)
      val lamId          = mkStaticDep(lam.getId)
      val lamArgXId      = mkStaticDep(lamArgX.getId)
      val mulId          = mkStaticDep(mul.getId)
      val mulFnId        = mkStaticDep(mulFn.getId)
      val mulArg1Id      = mkStaticDep(mulArg1.getId)
      val mulArg1ExprId  = mkStaticDep(mulArg1Expr.getId)
      val mulArg2Id      = mkStaticDep(mulArg2.getId)
      val mulArg2ExprId  = mkStaticDep(mulArg2Expr.getId)

      // Global Symbols
      val mulSym = mkDynamicDep("*")

      // The test
      depInfo.getDirect(appId) should not be defined
      depInfo.getDirect(appFnId) shouldEqual Some(Set(appId))
      depInfo.getDirect(appArg10Id) shouldEqual Some(Set(appId))
      depInfo.getDirect(appArg10ExprId) shouldEqual Some(Set(appArg10Id))
      depInfo.getDirect(appArg10NameId) shouldEqual Some(Set(appArg10Id))
      depInfo.getDirect(appArgFnId) shouldEqual Some(Set(appId))
      depInfo.getDirect(lamId) shouldEqual Some(Set(appArgFnId))
      depInfo.getDirect(lamArgXId) shouldEqual Some(
        Set(mulArg1ExprId, mulArg2ExprId)
      )
      depInfo.getDirect(mulId) shouldEqual Some(Set(lamId))
      depInfo.getDirect(mulFnId) shouldEqual Some(Set(mulId))
      depInfo.getDirect(mulArg1Id) shouldEqual Some(Set(mulId))
      depInfo.getDirect(mulArg1ExprId) shouldEqual Some(Set(mulArg1Id))
      depInfo.getDirect(mulArg2Id) shouldEqual Some(Set(mulId))
      depInfo.getDirect(mulArg2ExprId) shouldEqual Some(Set(mulArg2Id))

      depInfo.getDirect(mulSym) shouldEqual Some(Set(mulFnId))
    }

    "work properly for forces" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> x
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val lam = ir.asInstanceOf[IR.Function.Lambda]
      val argX =
        lam.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
      val xUse = lam.body.asInstanceOf[IR.Name.Literal]

      // The IDs
      val lamId  = mkStaticDep(lam.getId)
      val argXId = mkStaticDep(argX.getId)
      val xUseId = mkStaticDep(xUse.getId)

      // The Test
      depInfo.getDirect(argXId) shouldEqual Some(Set(xUseId))
      depInfo.getDirect(xUseId) shouldEqual Some(Set(lamId))
    }

    "work properly for blocks" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x = 10
          |x
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val block     = ir.asInstanceOf[IR.Expression.Block]
      val xBind     = block.expressions.head.asInstanceOf[IR.Expression.Binding]
      val xBindName = xBind.name.asInstanceOf[IR.Name.Literal]
      val xBindExpr = xBind.expression.asInstanceOf[IR.Literal.Number]
      val xUse      = block.returnValue.asInstanceOf[IR.Name.Literal]

      // The IDs
      val blockId     = mkStaticDep(block.getId)
      val xBindId     = mkStaticDep(xBind.getId)
      val xBindNameId = mkStaticDep(xBindName.getId)
      val xBindExprId = mkStaticDep(xBindExpr.getId)
      val xUseId      = mkStaticDep(xUse.getId)

      // The Test
      depInfo.getDirect(blockId) should not be defined
      depInfo.getDirect(xBindId) shouldEqual Some(Set(xUseId))
      depInfo.getDirect(xBindNameId) shouldEqual Some(Set(xBindId))
      depInfo.getDirect(xBindExprId) shouldEqual Some(Set(xBindId))
      depInfo.getDirect(xUseId) shouldEqual Some(Set(blockId))
    }

    "work properly for bindings" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x = 10
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val binding     = ir.asInstanceOf[IR.Expression.Binding]
      val bindingName = binding.name.asInstanceOf[IR.Name.Literal]
      val bindingExpr = binding.expression.asInstanceOf[IR.Literal.Number]

      // The IDs
      val bindingId     = mkStaticDep(binding.getId)
      val bindingNameId = mkStaticDep(bindingName.getId)
      val bindingExprId = mkStaticDep(bindingExpr.getId)

      // The Test
      depInfo.getDirect(bindingId) should not be defined
      depInfo.getDirect(bindingNameId) shouldEqual Some(Set(bindingId))
      depInfo.getDirect(bindingExprId) shouldEqual Some(Set(bindingId))
    }

    "work properly for undefined variables" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x = undefined
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val binding     = ir.asInstanceOf[IR.Expression.Binding]
      val bindingName = binding.name.asInstanceOf[IR.Name.Literal]
      val bindingExpr = binding.expression.asInstanceOf[IR.Error.Resolution]

      // The IDs
      val bindingId     = mkStaticDep(binding.getId)
      val bindingNameId = mkStaticDep(bindingName.getId)
      val bindingExprId = mkStaticDep(bindingExpr.getId)

      // The Test
      depInfo.getDirect(bindingId) should not be defined
      depInfo.getDirect(bindingNameId) shouldEqual Some(Set(bindingId))
      depInfo.getDirect(bindingExprId) shouldEqual Some(Set(bindingId))
    }

    "work properly for vector literals" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x -> [x, y * z + 1, 123]
          |""".stripMargin.preprocessExpression.get.analyse
          .asInstanceOf[IR.Function.Lambda]

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val callArg = ir.body
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
      val vector = callArg.value
        .asInstanceOf[IR.Application.Literal.Sequence]

      val xDefId    = mkStaticDep(ir.arguments(0).getId)
      val xUseId    = mkStaticDep(vector.items(0).getId)
      val yId       = mkStaticDep(vector.items(1).getId)
      val litId     = mkStaticDep(vector.items(2).getId)
      val vecId     = mkStaticDep(vector.getId)
      val callArgId = mkStaticDep(callArg.getId)
      val appId     = mkStaticDep(ir.body.getId)
      val lamId     = mkStaticDep(ir.getId)

      depInfo.getDirect(xDefId) shouldEqual Some(Set(xUseId))
      depInfo.getDirect(xUseId) shouldEqual Some(Set(vecId))
      depInfo.getDirect(yId) shouldEqual Some(Set(vecId))
      depInfo.getDirect(litId) shouldEqual Some(Set(vecId))
      depInfo.getDirect(vecId) shouldEqual Some(Set(callArgId))
      depInfo.getDirect(callArgId) shouldEqual Some(Set(appId))
      depInfo.getDirect(appId) shouldEqual Some(Set(lamId))
    }

    "work properly for typeset literals" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |{ x := a ; y := b }
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val literal           = ir.asInstanceOf[IR.Application.Literal.Typeset]
      val literalExpression = literal.expression.get

      val literalId           = mkStaticDep(literal.getId)
      val literalExpressionId = mkStaticDep(literalExpression.getId)

      depInfo.getDirect(literalExpressionId).get shouldEqual Set(literalId)
    }

    "work properly for case expressions" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |case foo x of
          |    Cons a b -> a + b
          |    _ -> 0
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val caseBlock = ir.asInstanceOf[IR.Expression.Block]
      val caseBinding =
        caseBlock.expressions.head.asInstanceOf[IR.Expression.Binding]
      val caseBindingExpr =
        caseBinding.expression.asInstanceOf[IR.Application.Prefix]
      val caseBindingName = caseBinding.name.asInstanceOf[IR.Name.Literal]
      val caseExpr        = caseBlock.returnValue.asInstanceOf[IR.Case.Expr]
      val scrutinee       = caseExpr.scrutinee.asInstanceOf[IR.Name.Literal]
      val consBranch      = caseExpr.branches.head

      val consBranchPattern =
        consBranch.pattern.asInstanceOf[Pattern.Constructor]
      val consBranchPatternCons = consBranchPattern.constructor
      val consBranchAPattern =
        consBranchPattern.fields.head.asInstanceOf[Pattern.Name]
      val consBranchADef = consBranchAPattern.name
      val consBranchBPattern =
        consBranchPattern.fields(1).asInstanceOf[Pattern.Name]
      val consBranchBDef = consBranchBPattern.name

      val consBranchExpression =
        consBranch.expression.asInstanceOf[IR.Application.Prefix]
      val aArg = consBranchExpression.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
      val aUse = aArg.value.asInstanceOf[IR.Name.Literal]
      val bArg = consBranchExpression
        .arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
      val bUse = bArg.value.asInstanceOf[IR.Name.Literal]

      // The IDs
      val caseBlockId       = mkStaticDep(caseBlock.getId)
      val caseBindingId     = mkStaticDep(caseBinding.getId)
      val caseBindingExprId = mkStaticDep(caseBindingExpr.getId)
      val caseBindingNameId = mkStaticDep(caseBindingName.getId)
      val caseExprId        = mkStaticDep(caseExpr.getId)
      val scrutineeId       = mkStaticDep(scrutinee.getId)
      val consBranchId      = mkStaticDep(consBranch.getId)

      val consBranchPatternId     = mkStaticDep(consBranchPattern.getId)
      val consBranchPatternConsId = mkStaticDep(consBranchPatternCons.getId)
      val consBranchAPatternId    = mkStaticDep(consBranchAPattern.getId)
      val consBranchADefId        = mkStaticDep(consBranchADef.getId)
      val consBranchBPatternId    = mkStaticDep(consBranchBPattern.getId)
      val consBranchBDefId        = mkStaticDep(consBranchBDef.getId)

      val consBranchExpressionId = mkStaticDep(consBranchExpression.getId)
      val aArgId                 = mkStaticDep(aArg.getId)
      val aUseId                 = mkStaticDep(aUse.getId)
      val bArgId                 = mkStaticDep(bArg.getId)
      val bUseId                 = mkStaticDep(bUse.getId)

      // The Test
      depInfo.getDirect(caseBlockId) should not be defined
      depInfo.getDirect(caseExprId) shouldEqual Some(Set(caseBlockId))
      depInfo.getDirect(scrutineeId) shouldEqual Some(Set(caseExprId))
      depInfo.getDirect(caseBindingId) shouldEqual Some(Set(scrutineeId))
      depInfo.getDirect(caseBindingExprId) shouldEqual Some(Set(caseBindingId))
      depInfo.getDirect(caseBindingNameId) shouldEqual Some(Set(caseBindingId))
      depInfo.getDirect(consBranchId) shouldEqual Some(Set(caseExprId))

      depInfo.getDirect(consBranchPatternId) shouldEqual Some(Set(consBranchId))
      depInfo.getDirect(consBranchPatternConsId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      depInfo.getDirect(consBranchAPatternId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      depInfo.getDirect(consBranchADefId) shouldEqual Some(
        Set(consBranchAPatternId, aUseId)
      )
      depInfo.getDirect(consBranchBPatternId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      depInfo.getDirect(consBranchBDefId) shouldEqual Some(
        Set(consBranchBPatternId, bUseId)
      )

      depInfo.getDirect(consBranchExpressionId) shouldEqual Some(
        Set(consBranchId)
      )
      depInfo.getDirect(aArgId) shouldEqual Some(Set(consBranchExpressionId))
      depInfo.getDirect(aUseId) shouldEqual Some(Set(aArgId))
      depInfo.getDirect(bArgId) shouldEqual Some(Set(consBranchExpressionId))
      depInfo.getDirect(bUseId) shouldEqual Some(Set(bArgId))
    }

    "have the result data associated with literals" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      "10".preprocessExpression.get.analyse
        .asInstanceOf[IR.Literal]
        .hasDependencyInfo
    }
  }

  "Dataflow analysis with external identifiers" should {
    implicit val inlineContext: InlineContext = mkInlineContext

    val meta     = new Metadata
    val lambdaId = meta.addItem(1, 59)
    val aBindId  = meta.addItem(10, 9)

    val code =
      """
        |x ->
        |    a = x + 1
        |    b = State.read
        |    a+b . IO.println
        |""".stripMargin

    val codeWithMeta = meta.appendToCode(code)
    val ir = codeWithMeta.preprocessExpression.get.analyse
      .asInstanceOf[IR.Function.Lambda]

    val metadata  = ir.getMetadata(DataflowAnalysis).get
    val blockBody = ir.body.asInstanceOf[IR.Expression.Block]

    val aBind = blockBody.expressions.head
      .asInstanceOf[IR.Expression.Binding]
    val aBindExpr = aBind.expression

    "store a mapping between internal and external identifiers" in {
      metadata.get(asStatic(aBind)).get should contain(
        asStatic(ir)
      )

      asStatic(ir).externalId shouldEqual Some(lambdaId)
    }

    "return the set of external identifiers for invalidation" in {
      metadata.getExternal(asStatic(aBindExpr)).get shouldEqual Set(
        lambdaId,
        aBindId
      )
    }

    "return the set of direct external identifiers for invalidation" in {
      metadata.getExternalDirect(asStatic(aBindExpr)).get shouldEqual Set(
        aBindId
      )
    }
  }
}

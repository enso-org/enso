package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.core.IR.Pattern
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo.Type.asStatic
import org.enso.compiler.pass.analyse.DataflowAnalysis.{
  DependencyInfo,
  DependencyMapping
}
import org.enso.compiler.pass.analyse.{AliasAnalysis, DataflowAnalysis}
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.interpreter.test.Metadata
import org.scalatest.Assertion

class DataflowAnalysisTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(CompilerConfig())

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
      val dependencies = new DependencyMapping
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
      val dependencies = new DependencyMapping
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
      val dependencies = new DependencyMapping
      val ids          = List.fill(5)(genStaticDep)

      dependencies(ids.head) = Set(ids(1), ids(2))
      dependencies(ids(2))   = Set(ids(3), ids(4))
      dependencies(ids(4))   = Set(ids(1), ids.head)

      dependencies.getDirect(ids.head) shouldEqual Some(Set(ids(1), ids(2)))
      dependencies.getDirect(ids(2)) shouldEqual Some(Set(ids(3), ids(4)))
      dependencies.getDirect(ids(4)) shouldEqual Some(Set(ids(1), ids.head))
    }

    "allow for updating the dependents of a node" in {
      val dependencies = new DependencyMapping
      val ids          = List.fill(3)(genStaticDep)

      dependencies(ids.head) = Set(ids(1))
      dependencies(ids.head) shouldEqual Set(ids(1))

      dependencies(ids.head) = Set(ids(2))
      dependencies(ids.head) shouldEqual Set(ids(2))

      dependencies(ids.head) ++= Set(ids(1))
      dependencies(ids.head) shouldEqual Set(ids(1), ids(2))
    }

    "allow for updating at a given node" in {
      val dependencies = new DependencyMapping
      val ids          = List.fill(6)(genStaticDep)
      val set1         = Set.from(ids.tail)
      val newId        = genStaticDep

      dependencies.updateAt(ids.head, set1)
      dependencies(ids.head) shouldEqual set1

      dependencies.updateAt(ids.head, Set(newId))
      dependencies(ids.head) shouldEqual (set1 + newId)
    }

    "allow combining the information from multiple modules" in {
      val module1 = new DependencyMapping
      val module2 = new DependencyMapping

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
      depInfo.dependents.getDirect(frobnicateSymbol) shouldEqual Some(
        Set(frobFnId)
      )
      depInfo.dependents.getDirect(ioSymbol) shouldEqual Some(
        Set(printlnArgIOExprId)
      )
      depInfo.dependents.getDirect(printlnSymbol) shouldEqual Some(
        Set(printlnFnId)
      )
      depInfo.dependents.getDirect(plusSymbol) shouldEqual Some(
        Set(plusExprFnId)
      )
    }

    "correctly identify global symbol direct dependencies" in {
      depInfo.dependencies.getDirect(frobnicateSymbol) shouldBe empty
      depInfo.dependencies.getDirect(ioSymbol) shouldBe empty
      depInfo.dependencies.getDirect(printlnSymbol) shouldBe empty
      depInfo.dependencies.getDirect(plusSymbol) shouldBe empty
    }

    "correctly identify global symbol indirect dependents" in {
      depInfo.dependents.get(frobnicateSymbol) shouldEqual Some(
        Set(frobFnId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(ioSymbol) shouldEqual Some(
        Set(printlnArgIOExprId, printlnArgIOId, printlnExprId)
      )
      depInfo.dependents.get(printlnSymbol) shouldEqual Some(
        Set(printlnFnId, printlnExprId)
      )
      depInfo.dependents.get(plusSymbol) shouldEqual Some(
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

    "correctly identify global symbol indirect dependencies" in {
      depInfo.dependencies.get(frobnicateSymbol) shouldBe empty
      depInfo.dependencies.get(ioSymbol) shouldBe empty
      depInfo.dependencies.get(printlnSymbol) shouldBe empty
      depInfo.dependencies.get(plusSymbol) shouldBe empty
    }

    "correctly identify local direct dependents" in {
      depInfo.dependents.getDirect(fnId) shouldEqual Some(Set(methodId))
      depInfo.dependents.getDirect(fnArgAId) shouldEqual Some(
        Set(plusExprArgAExprId, frobArgAExprId)
      )
      depInfo.dependents.getDirect(fnArgBId) shouldEqual Some(
        Set(printlnArgBExprId, plusExprArgBExprId)
      )
      depInfo.dependents.getDirect(fnBodyId) shouldEqual Some(Set(fnId))

      // The `IO.println` expression
      depInfo.dependents.getDirect(printlnExprId) should not be defined
      depInfo.dependents.getDirect(printlnFnId) shouldEqual Some(
        Set(printlnExprId)
      )
      depInfo.dependents.getDirect(printlnArgIOId) shouldEqual Some(
        Set(printlnExprId)
      )
      depInfo.dependents.getDirect(printlnArgIOExprId) shouldEqual Some(
        Set(printlnArgIOId)
      )
      depInfo.dependents.getDirect(printlnArgBId) shouldEqual Some(
        Set(printlnExprId)
      )
      depInfo.dependents.getDirect(printlnArgBExprId) shouldEqual Some(
        Set(printlnArgBId)
      )

      // The `c = ` expression
      depInfo.dependents.getDirect(cBindExprId) shouldEqual Some(
        Set(frobArgCExprId)
      )
      depInfo.dependents.getDirect(cBindNameId) shouldEqual Some(
        Set(cBindExprId)
      )
      depInfo.dependents.getDirect(plusExprId) shouldEqual Some(
        Set(cBindExprId)
      )
      depInfo.dependents.getDirect(plusExprFnId) shouldEqual Some(
        Set(plusExprId)
      )
      depInfo.dependents.getDirect(plusExprArgAId) shouldEqual Some(
        Set(plusExprId)
      )
      depInfo.dependents.getDirect(plusExprArgAExprId) shouldEqual Some(
        Set(plusExprArgAId)
      )
      depInfo.dependents.getDirect(plusExprArgBId) shouldEqual Some(
        Set(plusExprId)
      )
      depInfo.dependents.getDirect(plusExprArgBExprId) shouldEqual Some(
        Set(plusExprArgBId)
      )

      // The `frobnicate` expression
      depInfo.dependents.getDirect(frobExprId) shouldEqual Some(Set(fnBodyId))
      depInfo.dependents.getDirect(frobFnId) shouldEqual Some(Set(frobExprId))
      depInfo.dependents.getDirect(frobArgAId) shouldEqual Some(Set(frobExprId))
      depInfo.dependents.getDirect(frobArgAExprId) shouldEqual Some(
        Set(frobArgAId)
      )
      depInfo.dependents.getDirect(frobArgCId) shouldEqual Some(Set(frobExprId))
      depInfo.dependents.getDirect(frobArgCExprId) shouldEqual Some(
        Set(frobArgCId)
      )
    }

    "correctly identify local direct dependencies" in {
      val dependencies = depInfo.dependencies
      dependencies.getDirect(methodId) shouldEqual Some(Set(fnId))
      dependencies.getDirect(fnId) shouldEqual Some(Set(fnBodyId))
      dependencies.getDirect(fnBodyId) shouldEqual Some(Set(frobExprId))
      dependencies.getDirect(fnArgAId) shouldEqual None
      dependencies.getDirect(fnArgBId) shouldEqual None

      // The `IO.println` expression
      dependencies.getDirect(printlnExprId) shouldEqual Some(
        Set(printlnArgIOId, printlnArgBId, printlnFnId)
      )
      dependencies.getDirect(printlnFnId) shouldEqual Some(Set(printlnSymbol))
      dependencies.getDirect(printlnArgIOId) shouldEqual Some(
        Set(printlnArgIOExprId)
      )
      dependencies.getDirect(printlnArgBId) shouldEqual Some(
        Set(printlnArgBExprId)
      )
      dependencies.getDirect(printlnArgIOExprId) shouldEqual Some(Set(ioSymbol))
      dependencies.getDirect(printlnArgBExprId) shouldEqual Some(Set(fnArgBId))

      // The `c = ...` expression
      dependencies.getDirect(cBindExprId) shouldEqual Some(
        Set(cBindNameId, plusExprId)
      )
      dependencies.getDirect(cBindNameId) shouldEqual None
      dependencies.getDirect(plusExprId) shouldEqual Some(
        Set(plusExprFnId, plusExprArgAId, plusExprArgBId)
      )
      dependencies.getDirect(plusExprFnId) shouldEqual Some(Set(plusSymbol))
      dependencies.getDirect(plusExprArgAId) shouldEqual Some(
        Set(plusExprArgAExprId)
      )
      dependencies.getDirect(plusExprArgBId) shouldEqual Some(
        Set(plusExprArgBExprId)
      )
      dependencies.getDirect(plusExprArgAExprId) shouldEqual Some(Set(fnArgAId))
      dependencies.getDirect(plusExprArgBExprId) shouldEqual Some(Set(fnArgBId))

      // The `frobnicate` expression
      dependencies.getDirect(frobExprId) shouldEqual Some(
        Set(frobFnId, frobArgAId, frobArgCId)
      )
      dependencies.getDirect(frobFnId) shouldEqual Some(Set(frobnicateSymbol))
      dependencies.getDirect(frobArgAId) shouldEqual Some(Set(frobArgAExprId))
      dependencies.getDirect(frobArgCId) shouldEqual Some(Set(frobArgCExprId))
      dependencies.getDirect(frobArgAExprId) shouldEqual Some(Set(fnArgAId))
      dependencies.getDirect(frobArgCExprId) shouldEqual Some(Set(cBindExprId))
    }

    "correctly identify local indirect dependents" in {
      depInfo.dependents.get(fnId) shouldEqual Some(Set(methodId))
      depInfo.dependents.get(fnArgAId) shouldEqual Some(
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
      depInfo.dependents.get(fnArgBId) shouldEqual Some(
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
      depInfo.dependents.get(fnBodyId) shouldEqual Some(Set(fnId, methodId))

      // The `IO.println` expression
      depInfo.dependents.get(printlnExprId) should not be defined
      depInfo.dependents.get(printlnFnId) shouldEqual Some(Set(printlnExprId))
      depInfo.dependents.get(printlnArgIOId) shouldEqual Some(
        Set(printlnExprId)
      )
      depInfo.dependents.get(printlnArgIOExprId) shouldEqual Some(
        Set(printlnArgIOId, printlnExprId)
      )
      depInfo.dependents.get(printlnArgBId) shouldEqual Some(Set(printlnExprId))
      depInfo.dependents.get(printlnArgBExprId) shouldEqual Some(
        Set(printlnArgBId, printlnExprId)
      )

      // The `c = ` expression
      depInfo.dependents.get(cBindExprId) shouldEqual Some(
        Set(frobArgCExprId, frobArgCId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(cBindNameId) shouldEqual Some(
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
      depInfo.dependents.get(plusExprId) shouldEqual Some(
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
      depInfo.dependents.get(plusExprFnId) shouldEqual Some(
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
      depInfo.dependents.get(plusExprArgAId) shouldEqual Some(
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
      depInfo.dependents.get(plusExprArgAExprId) shouldEqual Some(
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

      depInfo.dependents.get(plusExprArgBId) shouldEqual Some(
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
      depInfo.dependents.get(plusExprArgBExprId) shouldEqual Some(
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
      depInfo.dependents.get(frobExprId) shouldEqual Some(
        Set(
          fnBodyId,
          fnId,
          methodId
        )
      )
      depInfo.dependents.get(frobFnId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(frobArgAId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(frobArgAExprId) shouldEqual Some(
        Set(frobArgAId, frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(frobArgCId) shouldEqual Some(
        Set(frobExprId, fnBodyId, fnId, methodId)
      )
      depInfo.dependents.get(frobArgCExprId) shouldEqual Some(
        Set(frobArgCId, frobExprId, fnBodyId, fnId, methodId)
      )
    }

    "correctly identify local indirect dependencies" in {
      val dependencies = depInfo.dependencies

      dependencies.get(methodId) shouldEqual Some(
        Set(
          fnId,
          fnBodyId,
          frobExprId,
          frobArgAId,
          frobArgAExprId,
          frobArgCId,
          frobArgCExprId,
          frobFnId,
          frobnicateSymbol,
          cBindExprId,
          cBindNameId,
          plusExprId,
          plusExprFnId,
          plusSymbol,
          plusExprArgAId,
          plusExprArgBId,
          plusExprArgAExprId,
          plusExprArgBExprId,
          fnArgAId,
          fnArgBId
        )
      )
      dependencies.get(fnId) shouldEqual Some(
        Set(
          fnBodyId,
          frobExprId,
          frobArgAId,
          frobArgAExprId,
          frobArgCId,
          frobArgCExprId,
          frobFnId,
          frobnicateSymbol,
          cBindExprId,
          cBindNameId,
          plusExprId,
          plusExprFnId,
          plusSymbol,
          plusExprArgAId,
          plusExprArgBId,
          plusExprArgAExprId,
          plusExprArgBExprId,
          fnArgAId,
          fnArgBId
        )
      )
      dependencies.get(fnBodyId) shouldEqual Some(
        Set(
          frobExprId,
          frobArgAId,
          frobArgAExprId,
          frobArgCId,
          frobArgCExprId,
          frobFnId,
          frobnicateSymbol,
          cBindExprId,
          cBindNameId,
          plusExprId,
          plusExprFnId,
          plusSymbol,
          plusExprArgAId,
          plusExprArgBId,
          plusExprArgAExprId,
          plusExprArgBExprId,
          fnArgAId,
          fnArgBId
        )
      )
      dependencies.get(fnArgAId) shouldEqual None
      dependencies.get(fnArgBId) shouldEqual None

      // The `IO.println` expression
      dependencies.get(printlnExprId) shouldEqual Some(
        Set(
          printlnFnId,
          printlnSymbol,
          printlnArgIOId,
          printlnArgIOExprId,
          ioSymbol,
          printlnArgBId,
          printlnArgBExprId,
          fnArgBId
        )
      )
      dependencies.get(printlnFnId) shouldEqual Some(Set(printlnSymbol))
      dependencies.get(printlnArgIOId) shouldEqual Some(
        Set(printlnArgIOExprId, ioSymbol)
      )
      dependencies.get(printlnArgIOExprId) shouldEqual Some(Set(ioSymbol))
      dependencies.get(printlnArgBId) shouldEqual Some(
        Set(printlnArgBExprId, fnArgBId)
      )
      dependencies.get(printlnArgBExprId) shouldEqual Some(Set(fnArgBId))

      // The `c = ...` expression
      dependencies.get(cBindExprId) shouldEqual Some(
        Set(
          cBindNameId,
          plusExprId,
          plusExprArgAId,
          plusExprArgAExprId,
          fnArgAId,
          plusExprFnId,
          plusSymbol,
          plusExprArgBId,
          plusExprArgBExprId,
          fnArgBId
        )
      )
      dependencies.get(cBindNameId) shouldEqual None
      dependencies.get(plusExprId) shouldEqual Some(
        Set(
          plusExprArgAId,
          plusExprArgAExprId,
          fnArgAId,
          plusExprFnId,
          plusSymbol,
          plusExprArgBId,
          plusExprArgBExprId,
          fnArgBId
        )
      )
      dependencies.get(plusExprArgAId) shouldEqual Some(
        Set(
          plusExprArgAExprId,
          fnArgAId
        )
      )
      dependencies.get(plusExprFnId) shouldEqual Some(Set(plusSymbol))
      dependencies.get(plusExprArgBId) shouldEqual Some(
        Set(plusExprArgBExprId, fnArgBId)
      )

      // The `frobnicate` expression
      dependencies.get(frobExprId) shouldEqual Some(
        Set(
          frobArgAId,
          frobArgAExprId,
          frobArgCId,
          frobArgCExprId,
          frobFnId,
          frobnicateSymbol,
          cBindExprId,
          cBindNameId,
          plusExprId,
          plusExprFnId,
          plusSymbol,
          plusExprArgAId,
          plusExprArgBId,
          plusExprArgAExprId,
          plusExprArgBExprId,
          fnArgAId,
          fnArgBId
        )
      )
      dependencies.get(frobArgAId) shouldEqual Some(
        Set(frobArgAExprId, fnArgAId)
      )
      dependencies.get(frobArgCId) shouldEqual Some(
        Set(
          frobArgCExprId,
          cBindExprId,
          cBindNameId,
          plusExprId,
          plusExprFnId,
          plusSymbol,
          plusExprArgAId,
          plusExprArgAExprId,
          fnArgAId,
          plusExprArgBId,
          plusExprArgBExprId,
          fnArgBId
        )
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

      // Info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The Tests for dependents
      dependents.getDirect(fnId) should not be defined
      dependents.getDirect(fnArgXId) shouldEqual Some(
        Set(plusArgXExprId, fnArgYDefaultId)
      )
      dependents.getDirect(fnArgYId) shouldEqual Some(Set(plusArgYExprId))
      dependents.getDirect(fnArgYDefaultId) shouldEqual Some(Set(fnArgYId))
      dependents.getDirect(fnBodyId) shouldEqual Some(Set(fnId))
      dependents.getDirect(plusSym) shouldEqual Some(Set(plusFnId))
      dependents.getDirect(plusArgXId) shouldEqual Some(Set(fnBodyId))
      dependents.getDirect(plusArgXExprId) shouldEqual Some(Set(plusArgXId))
      dependents.getDirect(plusArgYId) shouldEqual Some(Set(fnBodyId))
      dependents.getDirect(plusArgYExprId) shouldEqual Some(Set(plusArgYId))

      // The Tests for dependencies
      dependencies.getDirect(fnId) shouldEqual Some(Set(fnBodyId))
      dependencies.getDirect(fnArgXId) shouldEqual None
      dependencies.getDirect(fnArgYId) shouldEqual Some(Set(fnArgYDefaultId))
      dependencies.getDirect(fnArgYDefaultId) shouldEqual Some(Set(fnArgXId))
      dependencies.getDirect(fnBodyId) shouldEqual Some(
        Set(plusFnId, plusArgXId, plusArgYId)
      )
      dependencies.getDirect(plusArgXId) shouldEqual Some(Set(plusArgXExprId))
      dependencies.getDirect(plusArgYId) shouldEqual Some(Set(plusArgYExprId))
      dependencies.getDirect(plusArgXExprId) shouldEqual Some(Set(fnArgXId))
      dependencies.getDirect(plusArgYExprId) shouldEqual Some(Set(fnArgYId))
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
      val fooSym = mkDynamicDep("foo")

      // The Info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The tests for dependents
      dependents.getDirect(appId) should not be defined
      dependents.getDirect(appFnId) shouldEqual Some(Set(appId))
      dependents.getDirect(appArg10Id) shouldEqual Some(Set(appId))
      dependents.getDirect(appArg10ExprId) shouldEqual Some(Set(appArg10Id))
      dependents.getDirect(appArg10NameId) shouldEqual Some(Set(appArg10Id))
      dependents.getDirect(appArgFnId) shouldEqual Some(Set(appId))
      dependents.getDirect(lamId) shouldEqual Some(Set(appArgFnId))
      dependents.getDirect(lamArgXId) shouldEqual Some(
        Set(mulArg1ExprId, mulArg2ExprId)
      )
      dependents.getDirect(mulId) shouldEqual Some(Set(lamId))
      dependents.getDirect(mulFnId) shouldEqual Some(Set(mulId))
      dependents.getDirect(mulArg1Id) shouldEqual Some(Set(mulId))
      dependents.getDirect(mulArg1ExprId) shouldEqual Some(Set(mulArg1Id))
      dependents.getDirect(mulArg2Id) shouldEqual Some(Set(mulId))
      dependents.getDirect(mulArg2ExprId) shouldEqual Some(Set(mulArg2Id))
      dependents.getDirect(mulSym) shouldEqual Some(Set(mulFnId))

      // The tests for dependencies
      dependencies.getDirect(appId) shouldEqual Some(
        Set(appFnId, appArg10Id, appArgFnId)
      )
      dependencies.getDirect(appFnId) shouldEqual Some(Set(fooSym))
      dependencies.getDirect(appArg10Id) shouldEqual Some(
        Set(appArg10NameId, appArg10ExprId)
      )
      dependencies.getDirect(appArg10NameId) shouldEqual None
      dependencies.getDirect(appArg10ExprId) shouldEqual None
      dependencies.getDirect(appArgFnId) shouldEqual Some(Set(lamId))
      dependencies.getDirect(lamId) shouldEqual Some(Set(mulId))
      dependencies.getDirect(lamArgXId) shouldEqual None
      dependencies.getDirect(mulId) shouldEqual Some(
        Set(mulFnId, mulArg1Id, mulArg2Id)
      )
      dependencies.getDirect(mulArg1Id) shouldEqual Some(Set(mulArg1ExprId))
      dependencies.getDirect(mulArg1ExprId) shouldEqual Some(Set(lamArgXId))
      dependencies.getDirect(mulArg2Id) shouldEqual Some(Set(mulArg2ExprId))
      dependencies.getDirect(mulArg2ExprId) shouldEqual Some(Set(lamArgXId))
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

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The test for dependents
      dependents.getDirect(argXId) shouldEqual Some(Set(xUseId))
      dependents.getDirect(xUseId) shouldEqual Some(Set(lamId))

      // The test for dependencies
      dependencies.getDirect(argXId) shouldEqual None
      dependencies.getDirect(xUseId) shouldEqual Some(Set(argXId))
      dependencies.getDirect(lamId) shouldEqual Some(Set(xUseId))
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

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The test for dependents
      dependents.getDirect(blockId) should not be defined
      dependents.getDirect(xBindId) shouldEqual Some(Set(xUseId))
      dependents.getDirect(xBindNameId) shouldEqual Some(Set(xBindId))
      dependents.getDirect(xBindExprId) shouldEqual Some(Set(xBindId))
      dependents.getDirect(xUseId) shouldEqual Some(Set(blockId))

      // The test for dependencies
      dependencies.getDirect(blockId) shouldEqual Some(Set(xUseId))
      dependencies.getDirect(xUseId) shouldEqual Some(Set(xBindId))
      dependencies.getDirect(xBindId) shouldEqual Some(
        Set(xBindNameId, xBindExprId)
      )
      dependencies.getDirect(xBindNameId) shouldEqual None
      dependencies.getDirect(xBindExprId) shouldEqual None
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

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The tests for dependents
      dependents.getDirect(bindingId) should not be defined
      dependents.getDirect(bindingNameId) shouldEqual Some(Set(bindingId))
      dependents.getDirect(bindingExprId) shouldEqual Some(Set(bindingId))

      // The tests for dependencies
      dependencies.getDirect(bindingId) shouldEqual Some(
        Set(bindingNameId, bindingExprId)
      )
      dependencies.getDirect(bindingNameId) shouldEqual None
      dependencies.getDirect(bindingExprId) shouldEqual None
    }

    "work properly for undefined variables" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x = undefined
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val binding       = ir.asInstanceOf[IR.Expression.Binding]
      val bindingName   = binding.name.asInstanceOf[IR.Name.Literal]
      val bindingExpr   = binding.expression.asInstanceOf[IR.Error.Resolution]
      val undefinedName = bindingExpr.originalName

      // The IDs
      val bindingId       = mkStaticDep(binding.getId)
      val bindingNameId   = mkStaticDep(bindingName.getId)
      val bindingExprId   = mkStaticDep(bindingExpr.getId)
      val undefinedNameId = mkDynamicDep(undefinedName.name)

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The tests for dependents
      dependents.getDirect(bindingId) should not be defined
      dependents.getDirect(bindingNameId) shouldEqual Some(Set(bindingId))
      dependents.getDirect(bindingExprId) shouldEqual Some(Set(bindingId))
      dependents.getDirect(undefinedNameId) shouldEqual Some(Set(bindingExprId))

      // The tests for dependencies
      dependencies.getDirect(bindingId) shouldEqual Some(
        Set(bindingNameId, bindingExprId)
      )
      dependencies.getDirect(bindingNameId) shouldEqual None
      dependencies.getDirect(bindingExprId) shouldEqual Some(
        Set(undefinedNameId)
      )
    }

    "work properly for undefined variables in expressions" in {
      implicit val inlineContext: InlineContext = mkInlineContext

      val ir =
        """
          |x = 1 + undefined
          |""".stripMargin.preprocessExpression.get.analyse

      val depInfo = ir.getMetadata(DataflowAnalysis).get

      val binding     = ir.asInstanceOf[IR.Expression.Binding]
      val bindingName = binding.name.asInstanceOf[IR.Name.Literal]
      val bindingExpr = binding.expression.asInstanceOf[IR.Application.Prefix]
      val plusFn      = bindingExpr.function.asInstanceOf[IR.Name.Literal]
      val numArg =
        bindingExpr.arguments.head.asInstanceOf[IR.CallArgument.Specified]
      val numArgExpr = numArg.value.asInstanceOf[IR.Literal.Number]
      val undefinedArg =
        bindingExpr
          .arguments(1)
          .asInstanceOf[IR.CallArgument.Specified]
      val undefinedExpr = undefinedArg.value.asInstanceOf[IR.Error.Resolution]
      val undefinedName = undefinedExpr.originalName

      // The IDs
      val bindingId       = mkStaticDep(binding.getId)
      val bindingExprId   = mkStaticDep(bindingExpr.getId)
      val bindingNameId   = mkStaticDep(bindingName.getId)
      val plusFnId        = mkStaticDep(plusFn.getId)
      val numArgId        = mkStaticDep(numArg.getId)
      val numArgExprId    = mkStaticDep(numArgExpr.getId)
      val undefinedArgId  = mkStaticDep(undefinedArg.getId)
      val undefinedExprId = mkStaticDep(undefinedExpr.getId)

      val undefinedSym = mkDynamicDep(undefinedName.name)
      val plusSym      = mkDynamicDep("+")

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // The tests for dependents
      dependents.getDirect(bindingId) should not be defined
      dependents.getDirect(bindingNameId) shouldEqual Some(Set(bindingId))
      dependents.getDirect(bindingExprId) shouldEqual Some(Set(bindingId))
      dependents.getDirect(undefinedArgId) shouldEqual Some(Set(bindingExprId))
      dependents.getDirect(undefinedExprId) shouldEqual Some(
        Set(undefinedArgId)
      )
      dependents.getDirect(undefinedSym) shouldEqual Some(
        Set(undefinedExprId)
      )

      // The tests for dependencies
      dependencies.getDirect(bindingId) shouldEqual Some(
        Set(bindingNameId, bindingExprId)
      )
      dependencies.getDirect(bindingNameId) shouldEqual None
      dependencies.getDirect(bindingExprId) shouldEqual Some(
        Set(plusFnId, undefinedArgId, numArgId)
      )
      dependencies.getDirect(numArgId) shouldEqual Some(Set(numArgExprId))
      dependencies.getDirect(plusFnId) shouldEqual Some(Set(plusSym))
      dependencies.getDirect(undefinedArgId) shouldEqual Some(
        Set(undefinedExprId)
      )
      dependencies.getDirect(undefinedExprId) shouldEqual Some(
        Set(undefinedSym)
      )
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

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // Tests for dependents
      dependents.getDirect(xDefId) shouldEqual Some(Set(xUseId))
      dependents.getDirect(xUseId) shouldEqual Some(Set(vecId))
      dependents.getDirect(yId) shouldEqual Some(Set(vecId))
      dependents.getDirect(litId) shouldEqual Some(Set(vecId))
      dependents.getDirect(vecId) shouldEqual Some(Set(callArgId))
      dependents.getDirect(callArgId) shouldEqual Some(Set(appId))
      dependents.getDirect(appId) shouldEqual Some(Set(lamId))

      // Tests for dependencies
      dependencies.getDirect(vecId) shouldEqual Some(Set(xUseId, yId, litId))
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

      depInfo.dependents.getDirect(literalExpressionId).get shouldEqual Set(
        literalId
      )
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
      val catchAllbranch  = caseExpr.branches(1)

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
      val consBranchFn =
        consBranchExpression.function.asInstanceOf[IR.Name.Literal]
      val aArg = consBranchExpression.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
      val aUse = aArg.value.asInstanceOf[IR.Name.Literal]
      val bArg = consBranchExpression
        .arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
      val bUse = bArg.value.asInstanceOf[IR.Name.Literal]

      val consSym = mkDynamicDep("Cons")
      val plusSym = mkDynamicDep("+")

      // The IDs
      val caseBlockId       = mkStaticDep(caseBlock.getId)
      val caseBindingId     = mkStaticDep(caseBinding.getId)
      val caseBindingExprId = mkStaticDep(caseBindingExpr.getId)
      val caseBindingNameId = mkStaticDep(caseBindingName.getId)
      val caseExprId        = mkStaticDep(caseExpr.getId)
      val scrutineeId       = mkStaticDep(scrutinee.getId)
      val consBranchId      = mkStaticDep(consBranch.getId)
      val catchAllBranchId  = mkStaticDep(catchAllbranch.getId)

      val consBranchPatternId     = mkStaticDep(consBranchPattern.getId)
      val consBranchPatternConsId = mkStaticDep(consBranchPatternCons.getId)
      val consBranchAPatternId    = mkStaticDep(consBranchAPattern.getId)
      val consBranchADefId        = mkStaticDep(consBranchADef.getId)
      val consBranchBPatternId    = mkStaticDep(consBranchBPattern.getId)
      val consBranchBDefId        = mkStaticDep(consBranchBDef.getId)

      val consBranchExpressionId = mkStaticDep(consBranchExpression.getId)
      val consBranchFnId         = mkStaticDep(consBranchFn.getId)
      val aArgId                 = mkStaticDep(aArg.getId)
      val aUseId                 = mkStaticDep(aUse.getId)
      val bArgId                 = mkStaticDep(bArg.getId)
      val bUseId                 = mkStaticDep(bUse.getId)

      // The info
      val dependents   = depInfo.dependents
      val dependencies = depInfo.dependencies

      // Tests for dependents
      dependents.getDirect(caseBlockId) should not be defined
      dependents.getDirect(caseExprId) shouldEqual Some(Set(caseBlockId))
      dependents.getDirect(scrutineeId) shouldEqual Some(Set(caseExprId))
      dependents.getDirect(caseBindingId) shouldEqual Some(Set(scrutineeId))
      dependents.getDirect(caseBindingExprId) shouldEqual Some(
        Set(caseBindingId)
      )
      dependents.getDirect(caseBindingNameId) shouldEqual Some(
        Set(caseBindingId)
      )
      dependents.getDirect(consBranchId) shouldEqual Some(Set(caseExprId))

      dependents.getDirect(consBranchPatternId) shouldEqual Some(
        Set(consBranchId)
      )
      dependents.getDirect(consBranchPatternConsId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      dependents.getDirect(consBranchAPatternId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      dependents.getDirect(consBranchADefId) shouldEqual Some(
        Set(consBranchAPatternId, aUseId)
      )
      dependents.getDirect(consBranchBPatternId) shouldEqual Some(
        Set(consBranchPatternId)
      )
      dependents.getDirect(consBranchBDefId) shouldEqual Some(
        Set(consBranchBPatternId, bUseId)
      )

      dependents.getDirect(consBranchExpressionId) shouldEqual Some(
        Set(consBranchId)
      )
      dependents.getDirect(aArgId) shouldEqual Some(Set(consBranchExpressionId))
      dependents.getDirect(aUseId) shouldEqual Some(Set(aArgId))
      dependents.getDirect(bArgId) shouldEqual Some(Set(consBranchExpressionId))
      dependents.getDirect(bUseId) shouldEqual Some(Set(bArgId))

      // Tests for dependencies
      dependencies.getDirect(caseBlockId) shouldEqual Some(Set(caseExprId))
      dependencies.getDirect(caseBindingId) shouldEqual Some(
        Set(caseBindingNameId, caseBindingExprId)
      )
      dependencies.getDirect(caseExprId) shouldEqual Some(
        Set(scrutineeId, consBranchId, catchAllBranchId)
      )
      dependencies.getDirect(scrutineeId) shouldEqual Some(Set(caseBindingId))
      dependencies.getDirect(consBranchId) shouldEqual Some(
        Set(consBranchPatternId, consBranchExpressionId)
      )

      dependencies.getDirect(consBranchPatternId) shouldEqual Some(
        Set(consBranchPatternConsId, consBranchAPatternId, consBranchBPatternId)
      )
      dependencies.getDirect(consBranchPatternConsId) shouldEqual Some(
        Set(consSym)
      )
      dependencies.getDirect(consBranchAPatternId) shouldEqual Some(
        Set(consBranchADefId)
      )
      dependencies.getDirect(consBranchBPatternId) shouldEqual Some(
        Set(consBranchBDefId)
      )

      dependencies.getDirect(consBranchExpressionId) shouldEqual Some(
        Set(aArgId, consBranchFnId, bArgId)
      )
      dependencies.getDirect(consBranchFnId) shouldEqual Some(Set(plusSym))
      dependencies.getDirect(aArgId) shouldEqual Some(Set(aUseId))
      dependencies.getDirect(aUseId) shouldEqual Some(Set(consBranchADefId))
      dependencies.getDirect(bArgId) shouldEqual Some(Set(bUseId))
      dependencies.getDirect(bUseId) shouldEqual Some(Set(consBranchBDefId))
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
      metadata.dependents.get(asStatic(aBind)).get should contain(
        asStatic(ir)
      )

      asStatic(ir).externalId shouldEqual Some(lambdaId)
    }

    "return the set of external identifiers for invalidation" in {
      metadata.dependents.getExternal(asStatic(aBindExpr)).get shouldEqual Set(
        lambdaId,
        aBindId
      )
    }

    "return the set of direct external identifiers for invalidation" in {
      metadata.dependents
        .getExternalDirect(asStatic(aBindExpr))
        .get shouldEqual Set(
        aBindId
      )
    }
  }

  "Dataflow analysis of conversions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |Foo.from (that : Bar) =
        |    Foo that 1
        |""".stripMargin.preprocessModule.analyse

    val depInfo = ir.getMetadata(DataflowAnalysis).get

    // The method and its body
    val conversion = ir.bindings.head.asInstanceOf[Method.Conversion]
    val sourceType = conversion.sourceTypeName.asInstanceOf[IR.Name]
    val lambda     = conversion.body.asInstanceOf[IR.Function.Lambda]
    val fnArgThis =
      lambda.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
    val fnArgValue =
      lambda.arguments(1).asInstanceOf[IR.DefinitionArgument.Specified]
    val fnBody = lambda.body.asInstanceOf[IR.Expression.Block]

    // The `Foo` application
    val fooExpr     = fnBody.returnValue.asInstanceOf[IR.Application.Prefix]
    val fooFunction = fooExpr.function.asInstanceOf[IR.Name]
    val fooArg1     = fooExpr.arguments.head.asInstanceOf[IR.CallArgument.Specified]
    val fooArg1Expr = fooArg1.value.asInstanceOf[IR.Name]
    val fooArg2     = fooExpr.arguments(1).asInstanceOf[IR.CallArgument.Specified]
    val fooArg2Expr = fooArg2.value.asInstanceOf[IR.Literal.Number]

    // The global symbols
    val fooSymbol = mkDynamicDep("Foo")

    // The identifiers
    val conversionId  = mkStaticDep(conversion.getId)
    val sourceTypeId  = mkStaticDep(sourceType.getId)
    val lambdaId      = mkStaticDep(lambda.getId)
    val fnArgThisId   = mkStaticDep(fnArgThis.getId)
    val fnArgValueId  = mkStaticDep(fnArgValue.getId)
    val fnBodyId      = mkStaticDep(fnBody.getId)
    val fooExprId     = mkStaticDep(fooExpr.getId)
    val fooFunctionId = mkStaticDep(fooFunction.getId)
    val fooArg1Id     = mkStaticDep(fooArg1.getId)
    val fooArg1ExprId = mkStaticDep(fooArg1Expr.getId)
    val fooArg2Id     = mkStaticDep(fooArg2.getId)
    val fooArg2ExprId = mkStaticDep(fooArg2Expr.getId)

    // The info
    val dependents   = depInfo.dependents
    val dependencies = depInfo.dependencies

    "correctly identify global symbol direct dependents" in {
      dependents.getDirect(fooSymbol) shouldEqual Some(Set(fooFunctionId))
    }

    "correctly identify global symbol direct dependencies" in {
      dependencies.getDirect(fooSymbol) shouldBe empty
    }

    "correctly identify local direct dependents" in {
      dependents.getDirect(conversionId) shouldBe empty
      dependents.getDirect(sourceTypeId) shouldEqual Some(Set(conversionId))
      dependents.getDirect(lambdaId) shouldEqual Some(Set(conversionId))
      dependents.getDirect(fnArgThisId) shouldBe empty
      dependents.getDirect(fnArgValueId) shouldBe Some(Set(fooArg1ExprId))
      dependents.getDirect(fnBodyId) shouldBe Some(Set(lambdaId))
      dependents.getDirect(fooExprId) shouldBe Some(Set(fnBodyId))
      dependents.getDirect(fooFunctionId) shouldBe Some(Set(fooExprId))
      dependents.getDirect(fooArg1Id) shouldBe Some(Set(fooExprId))
      dependents.getDirect(fooArg1ExprId) shouldBe Some(Set(fooArg1Id))
      dependents.getDirect(fooArg2Id) shouldBe Some(Set(fooExprId))
      dependents.getDirect(fooArg2ExprId) shouldBe Some(Set(fooArg2Id))
    }

    "correctly identify local direct dependencies" in {
      dependencies.getDirect(conversionId) shouldEqual Some(
        Set(sourceTypeId, lambdaId)
      )
      dependencies.getDirect(sourceTypeId) shouldBe empty
      dependencies.getDirect(lambdaId) shouldEqual Some(Set(fnBodyId))
      dependencies.getDirect(fnBodyId) shouldEqual Some(Set(fooExprId))
      dependencies.getDirect(fooExprId) shouldEqual Some(
        Set(fooFunctionId, fooArg1Id, fooArg2Id)
      )
      dependencies.getDirect(fooFunctionId) shouldEqual Some(Set(fooSymbol))
      dependencies.getDirect(fooArg1Id) shouldEqual Some(Set(fooArg1ExprId))
      dependencies.getDirect(fooArg2Id) shouldEqual Some(Set(fooArg2ExprId))
      dependencies.getDirect(fooArg1ExprId) shouldEqual Some(Set(fnArgValueId))
      dependencies.getDirect(fooArg2ExprId) shouldBe empty
    }

    "associate the dependency info with every node in the IR" in {
      conversion.hasDependencyInfo
      sourceType.hasDependencyInfo
      lambda.hasDependencyInfo
      fnArgThis.hasDependencyInfo
      fnArgValue.hasDependencyInfo
      fnBody.hasDependencyInfo
      fooExpr.hasDependencyInfo
      fooFunction.hasDependencyInfo
      fooArg1.hasDependencyInfo
      fooArg1Expr.hasDependencyInfo
      fooArg2.hasDependencyInfo
      fooArg2Expr.hasDependencyInfo
    }
  }
}

package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  CallArgument,
  Expression,
  Function,
  Module,
  Name,
  Pattern
}
import org.enso.compiler.core.ir.expression.{errors, Application, Case}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.analyse.AliasAnalysis.Graph.{Link, Occurrence}
import org.enso.compiler.pass.analyse.AliasAnalysis.{Graph, Info}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class AliasAnalysisTest extends CompilerTest {

  // === Utilities ============================================================

  val passes = new Passes(defaultConfig)

  /** The passes that need to be run before the alias analysis pass. */
  val precursorPasses: PassGroup = passes.getPrecursors(AliasAnalysis).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration(true)
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run alias analysis on an [[Module]].
    *
    * @param ir the module to run alias analysis on
    */
  implicit class AnalyseModule(ir: Module) {

    /** Runs alias analysis on a module.
      *
      * @return [[ir]], with attached aliasing information
      */
    def analyse: Module = {
      AliasAnalysis.runModule(
        ir,
        buildModuleContext(passConfiguration = Some(passConfig))
      )
    }
  }

  /** Adds an extension method to run alias analysis on an [[Expression]].
    *
    * @param ir the expression to run alias analysis on
    */
  implicit class AnalyseExpression(ir: Expression) {

    /** Runs alias analysis on an expression.
      *
      * @param inlineContext the inline context in which to process the
      *                      expression
      * @return [[ir]], with attached aliasing information
      */
    def analyse(inlineContext: InlineContext): Expression = {
      AliasAnalysis.runExpression(ir, inlineContext)
    }
  }

  /** Makes a new module context for testing purposes.
    *
    * @return a module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "The analysis scope" should {
    val graph = new Graph()

    val flatScope = new Graph.Scope()

    val complexScope        = new Graph.Scope()
    val child1              = complexScope.addChild()
    val child2              = complexScope.addChild()
    val childOfChild        = child1.addChild()
    val childOfChildOfChild = childOfChild.addChild()

    val aDefId = graph.nextId()
    val aDef   = Occurrence.Def(aDefId, "a", genId, None)

    val bDefId = graph.nextId()
    val bDef   = Occurrence.Def(bDefId, "b", genId, None)

    val aUseId = graph.nextId()
    val aUse   = Occurrence.Use(aUseId, "a", genId, None)

    val bUseId = graph.nextId()
    val bUse   = Occurrence.Use(bUseId, "b", genId, None)

    val cUseId = graph.nextId()
    val cUse   = Occurrence.Use(cUseId, "c", genId, None)

    // Add occurrences to the scopes
    complexScope.add(aDef)
    child2.add(cUse)
    childOfChild.add(bDef)
    childOfChild.add(bUse)
    childOfChildOfChild.add(aUse)

    "have a number of scopes of 1 without children" in {
      flatScope.scopeCount shouldEqual 1
    }

    "have a nesting level of 1 without children" in {
      flatScope.maxNesting shouldEqual 1
    }

    "have the correct number of scopes with children" in {
      complexScope.scopeCount shouldEqual 5
    }

    "have the correct nesting depth with children" in {
      complexScope.maxNesting shouldEqual 4
    }

    "allow correctly getting the n-th parent" in {
      childOfChildOfChild.nThParent(2) shouldEqual Some(child1)
    }

    "return `None` for nonexistent parents" in {
      childOfChildOfChild.nThParent(10) shouldEqual None
    }

    "find the occurrence for an ID in the current scope if it exists" in {
      complexScope.getOccurrence(aDefId) shouldEqual Some(aDef)
    }

    "find the occurrence for an name in the current scope if it exists" in {
      complexScope.getOccurrences[Occurrence](aDef.symbol) shouldEqual Set(aDef)
    }

    "find no occurrences if they do not exist" in {
      complexScope.getOccurrence(cUseId) shouldEqual None
    }

    "correctly resolve usage links where they exist" in {
      childOfChild.resolveUsage(bUse) shouldEqual Some(Link(bUseId, 0, bDefId))
      childOfChildOfChild.resolveUsage(aUse) shouldEqual Some(
        Link(aUseId, 3, aDefId)
      )
    }

    "correctly find the scope where a given ID occurs" in {
      complexScope.scopeFor(aUseId) shouldEqual Some(childOfChildOfChild)
    }

    "correctly find the scopes in which a given symbol occurs" in {
      complexScope.scopesForSymbol[Occurrence.Def]("a").length shouldEqual 1
      complexScope.scopesForSymbol[Occurrence.Use]("a").length shouldEqual 1

      complexScope.scopesForSymbol[Occurrence]("a").length shouldEqual 2

      complexScope.scopesForSymbol[Occurrence]("a") shouldEqual List(
        complexScope,
        childOfChildOfChild
      )
    }

    "return the correct set of symbols" in {
      complexScope.symbols shouldEqual Set("a", "b", "c")
    }

    "check correctly for specified occurrences of a symbol" in {
      complexScope.hasSymbolOccurrenceAs[Occurrence.Def]("a") shouldEqual true
      complexScope.hasSymbolOccurrenceAs[Occurrence]("b") shouldEqual false
    }

    "be able to convert from a symbol to identifiers that use it" in {
      complexScope.symbolToIds[Occurrence]("a") shouldEqual List(aDefId, aUseId)
    }

    "be able to convert from an identifier to the associated symbol" in {
      complexScope.idToSymbol(aDefId) shouldEqual Some("a")
    }

    "be able to check if a provided scope is a child of the current scope" in {
      child1.isChildOf(complexScope) shouldEqual true
      child2.isChildOf(complexScope) shouldEqual true
      childOfChild.isChildOf(complexScope) shouldEqual true

      complexScope.isChildOf(child1) shouldEqual false
    }

    "allow itself to be copied deeply" in {
      val complexScopeCopy = complexScope.deepCopy()

      complexScopeCopy shouldEqual complexScope
    }

    "count the number of scopes to the root" in {
      childOfChildOfChild.scopesToRoot shouldEqual 3
      childOfChild.scopesToRoot shouldEqual 2
      child1.scopesToRoot shouldEqual 1
      child2.scopesToRoot shouldEqual 1
      complexScope.scopesToRoot shouldEqual 0
    }
  }

  "The Aliasing graph" should {
    val graph = new Graph()

    val rootScope  = graph.rootScope
    val childScope = rootScope.addChild()

    val aDefId = graph.nextId()
    val aDef   = Occurrence.Def(aDefId, "a", genId, None)

    val bDefId = graph.nextId()
    val bDef   = Occurrence.Def(bDefId, "b", genId, None)

    val aUse1Id = graph.nextId()
    val aUse1   = Occurrence.Use(aUse1Id, "a", genId, None)

    val aUse2Id = graph.nextId()
    val aUse2   = Occurrence.Use(aUse2Id, "a", genId, None)

    val cUseId = graph.nextId()
    val cUse   = Occurrence.Use(cUseId, "c", genId, None)

    rootScope.add(aDef)
    rootScope.add(aUse1)
    rootScope.add(bDef)

    childScope.add(aUse2)
    childScope.add(cUse)

    val use1Link = graph.resolveLocalUsage(aUse1)
    val use2Link = graph.resolveLocalUsage(aUse2)
    val cUseLink = graph.resolveLocalUsage(cUse)

    "allow itself to be deep copied" in {
      val graphCopy = graph.copy

      graphCopy shouldEqual graph
    }

    "generate monotonically increasing identifiers" in {
      val ids       = List.fill(100)(graph.nextId())
      var currentId = ids.head - 1

      ids.forall(id => {
        currentId += 1
        currentId == id
      }) shouldEqual true
    }

    "correctly resolve usages where possible" in {
      use1Link shouldBe defined
      use2Link shouldBe defined
      cUseLink shouldBe empty

      use1Link.foreach { link =>
        {
          link.source shouldEqual aUse1Id
          link.target shouldEqual aDefId
          link.scopeCount shouldEqual 0
        }
      }

      use2Link.foreach { link =>
        {
          link.source shouldEqual aUse2Id
          link.target shouldEqual aDefId
          link.scopeCount shouldEqual 1
        }
      }
    }

    "gather all links for a given ID correctly" in {
      val linksForA = graph.linksFor(aDefId)
      val linksForC = graph.linksFor(cUseId)

      linksForA.size shouldEqual 2
      linksForC shouldBe empty

      linksForA should contain(use1Link.get)
      linksForA should contain(use2Link.get)
    }

    "find the scope where a given ID is defined" in {
      val scopeForA       = graph.scopeFor(aDefId)
      val scopeForUndefId = graph.scopeFor(100)

      scopeForA shouldBe defined
      scopeForUndefId shouldBe empty

      scopeForA shouldEqual Some(rootScope)
    }

    "find all scopes where a given symbol occurs" in {
      val aDefs = graph.scopesFor[Occurrence.Def]("a")
      val aUses = graph.scopesFor[Occurrence.Use]("a")
      val aOccs = graph.scopesFor[Occurrence]("a")
      val dOccs = graph.scopesFor[Occurrence]("d")

      aDefs.length shouldEqual 1
      aUses.length shouldEqual 2
      aOccs.length shouldEqual 2
      dOccs.length shouldEqual 0

      aDefs shouldEqual List(rootScope)
      aUses shouldEqual List(rootScope, childScope)
      aOccs shouldEqual List(rootScope, childScope)
    }

    "correctly determine the number of scopes in the graph" in {
      graph.numScopes shouldEqual 2
    }

    "correctly determine the maximum level of nesting in the graph" in {
      graph.nesting shouldEqual 2
    }

    "correctly determines whether an occurrence shadows other bindings" in {
      graph.canShadow(aDefId) shouldEqual true
      graph.canShadow(aUse1Id) shouldEqual false
    }

    "correctly determine the identifiers of bindings shadowed by a definition" in {
      val graph = new Graph()

      val rootScope  = graph.rootScope
      val child1     = rootScope.addChild()
      val child2     = rootScope.addChild()
      val grandChild = child1.addChild()

      val aDefInRootId = graph.nextId()
      val aDefInRoot   = Occurrence.Def(aDefInRootId, "a", genId, None)
      rootScope.add(aDefInRoot)

      val aDefInChild1Id = graph.nextId()
      val aDefInChild1   = Occurrence.Def(aDefInChild1Id, "a", genId, None)
      child1.add(aDefInChild1)

      val aDefInChild2Id = graph.nextId()
      val aDefInChild2   = Occurrence.Def(aDefInChild2Id, "a", genId, None)
      child2.add(aDefInChild2)

      val aDefInGrandChildId = graph.nextId()
      val aDefInGrandChild =
        Occurrence.Def(aDefInGrandChildId, "a", genId, None)
      grandChild.add(aDefInGrandChild)

      val bDefInRootId = graph.nextId()
      val bDefInRoot   = Occurrence.Def(bDefInRootId, "b", genId, None)
      rootScope.add(bDefInRoot)

      val bDefInChild2Id = graph.nextId()
      val bDefInChild2   = Occurrence.Def(bDefInChild2Id, "b", genId, None)
      child2.add(bDefInChild2)

      graph.knownShadowedDefinitions(aDefInGrandChild) shouldEqual Set(
        aDefInRoot,
        aDefInChild1
      )
      graph.knownShadowedDefinitions(aDefInChild1) shouldEqual Set(aDefInRoot)
      graph.knownShadowedDefinitions(aDefInRoot) shouldBe empty
      graph.knownShadowedDefinitions(aDefInChild2) shouldEqual Set(aDefInRoot)
      graph.knownShadowedDefinitions(bDefInChild2) shouldEqual Set(bDefInRoot)
      graph.knownShadowedDefinitions(bDefInRoot) shouldBe empty
    }

    "correctly determine all symbols that occur in the graph" in {
      graph.symbols shouldEqual Set("a", "b", "c")
    }

    "correctly return all links for a given symbol" in {
      graph.linksFor[Occurrence]("a") shouldEqual Set(
        use1Link.get,
        use2Link.get
      )
    }

    "correctly finds the occurrence for a provided ID" in {
      graph.getOccurrence(aDefId) shouldEqual Some(aDef)
      graph.getOccurrence(aUse1Id) shouldEqual Some(aUse1)
      graph.getOccurrence(cUseId) shouldEqual Some(cUse)
    }

    "correctly finds the definition link for a provided id" in {
      graph.defLinkFor(aUse1Id) shouldEqual use1Link
      graph.defLinkFor(aUse2Id) shouldEqual use2Link
    }
  }

  "Alias analysis for atom definitions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val goodAtom =
      """
        |type M
        |    MyAtom a b (c=a)
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[Definition.Type]
        .members
        .head
    val goodMeta  = goodAtom.getMetadata(AliasAnalysis)
    val goodGraph = goodMeta.get.unsafeAs[AliasAnalysis.Info.Scope.Root].graph

    val badAtom =
      """
        |type M
        |    MyAtom a=b b
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[Definition.Type]
        .members
        .head
    val badMeta  = badAtom.getMetadata(AliasAnalysis)
    val badGraph = badMeta.get.unsafeAs[AliasAnalysis.Info.Scope.Root].graph

    "assign Info.Scope.Root metadata to the atom" in {
      goodMeta shouldBe defined
      goodMeta.get shouldBe a[Info.Scope.Root]
    }

    "create definition occurrences in the atom's scope" in {
      goodGraph.rootScope.hasSymbolOccurrenceAs[Occurrence.Def]("a")
      goodGraph.rootScope.hasSymbolOccurrenceAs[Occurrence.Def]("b")
      goodGraph.rootScope.hasSymbolOccurrenceAs[Occurrence.Def]("c")
    }

    "create defaults in the same scope as the argument" in {
      goodGraph.nesting shouldEqual 1
      goodGraph.rootScope.hasSymbolOccurrenceAs[Occurrence.Use]("a")
    }

    "create usage links where valid" in {
      val aDefId = goodAtom.arguments.head
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      val aUseId = goodAtom
        .arguments(2)
        .defaultValue
        .get
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      goodGraph.links should contain(Link(aUseId, 0, aDefId))
    }

    "enforce the ordering scope constraint on function arguments" in {
      badGraph.links shouldBe empty
    }
  }

  "Alias analysis on function methods" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val methodWithLambda =
      """
        |Bar.foo = a -> b -> c ->
        |    d = a -> b -> a b
        |    g =
        |        1 + 1
        |    d c (a + b)
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]
    val methodWithLambdaGraph =
      methodWithLambda
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Root]
        .graph

    val graphLinks = methodWithLambdaGraph.links

    val topLambda = methodWithLambda.body.asInstanceOf[Function.Lambda]
    val topLambdaBody = topLambda.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]
    val childLambda =
      topLambdaBody.expressions.head
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Function.Lambda]
    val childLambdaBody = childLambda.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Application.Prefix]

    "assign Info.Scope.Root metadata to the method" in {
      val meta = methodWithLambda.getMetadata(AliasAnalysis)

      meta shouldBe defined
      meta.get shouldBe a[Info.Scope.Root]
    }

    "assign Info.Scope.Child to all child scopes" in {
      methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]

      methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]

      methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]

      topLambdaBody
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]

      childLambda.body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]
    }

    "not allocate additional scopes unnecessarily" in {
      methodWithLambdaGraph.nesting shouldEqual 6
      methodWithLambdaGraph.numScopes shouldEqual 11

      // TODO [AA] the method function's scope should be the block scope

      val cLamScope = methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val cLamBlockScope =
        methodWithLambda.body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Expression.Block]
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope

      cLamScope shouldEqual cLamBlockScope

      val aLamScope = methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      aLamScope shouldEqual methodWithLambdaGraph.rootScope
    }

    "allocate new scopes where necessary" in {
      val topScope =
        methodWithLambda.body
          .asInstanceOf[Function.Lambda]
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope

      val bLambdaScope = methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val cLambdaScope = methodWithLambda.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val mainBlockScope = topLambdaBody
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val dALambdaScope = topLambdaBody.expressions.head
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val gScope = topLambdaBody
        .expressions(1)
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Expression.Block]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      val cUseScope = topLambdaBody.returnValue
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      topScope.childScopes should contain(bLambdaScope)
      bLambdaScope.childScopes should contain(cLambdaScope)
      mainBlockScope.childScopes should contain(dALambdaScope)
      mainBlockScope.childScopes should contain(gScope)
      mainBlockScope.childScopes should contain(cUseScope)
    }

    "assign Info.Occurrence to definitions and usages of symbols" in {
      topLambda.arguments.foreach(arg =>
        arg.getMetadata(AliasAnalysis).get.as[Info.Occurrence] shouldBe defined
      )

      topLambdaBody.expressions.foreach(
        _.asInstanceOf[Expression.Binding]
          .getMetadata(AliasAnalysis)
          .get
          .as[Info.Occurrence] shouldBe defined
      )

      childLambda.arguments.foreach(arg =>
        arg.getMetadata(AliasAnalysis).get.as[Info.Occurrence] shouldBe defined
      )

      childLambdaBody.function
        .getMetadata(AliasAnalysis)
        .get
        .as[Info.Occurrence] shouldBe defined

      childLambdaBody.arguments.foreach(
        _.getMetadata(AliasAnalysis).get.as[Info.Scope.Child] shouldBe defined
      )
    }

    "create the correct usage links for resolvable entities" in {
      val topLambdaCDefId =
        topLambda.body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Function.Lambda]
          .arguments
          .head
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id

      val nestedLambdaADefId =
        childLambda.arguments.head
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id
      val nestedLambdaBDefId =
        childLambda.body
          .asInstanceOf[Function.Lambda]
          .arguments
          .head
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id

      val nestedLambdaAUseId = childLambdaBody
        .asInstanceOf[Application.Prefix]
        .function
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      val nestedLambdaBUseId = childLambdaBody
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      val dDefId = topLambdaBody.expressions.head
        .asInstanceOf[Expression.Binding]
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      val dUseId = topLambdaBody.returnValue
        .asInstanceOf[Application.Prefix]
        .function
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      val dDefCUseId = topLambdaBody.returnValue
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      graphLinks should contain(Link(nestedLambdaAUseId, 1, nestedLambdaADefId))
      graphLinks should contain(Link(nestedLambdaBUseId, 1, nestedLambdaBDefId))

      graphLinks should contain(Link(dUseId, 0, dDefId))
      graphLinks should contain(Link(dDefCUseId, 1, topLambdaCDefId))
    }

    "not resolve links for unknown symbols" in {
      val unknownPlusId = topLambdaBody
        .expressions(1)
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Application.Prefix]
        .function
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      methodWithLambdaGraph.linksFor(unknownPlusId) shouldBe empty
      methodWithLambdaGraph
        .getOccurrence(unknownPlusId)
        .get shouldBe an[Occurrence.Use]
    }
  }

  "Alias analysis on block methods" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val methodWithBlock =
      """
        |Bar.foo =
        |    a = 2 + 2
        |    b = a * a
        |
        |    IO.println b
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]
    val methodWithBlockGraph =
      methodWithBlock
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Root]
        .graph

    "assign Info.Scope.Root metadata to the method" in {
      val meta1 = methodWithBlock.getMetadata(AliasAnalysis)

      meta1 shouldBe defined
      meta1.get shouldBe a[Info.Scope.Root]
    }

    "assign Info.Scope.Child to all child scopes" in {
      methodWithBlock.body
        .asInstanceOf[Function.Lambda]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]

      methodWithBlock.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .getMetadata(AliasAnalysis)
        .get shouldBe an[Info.Scope.Child]
    }

    "not allocate additional scopes unnecessarily" in {
      methodWithBlockGraph.nesting shouldEqual 2
      methodWithBlockGraph.numScopes shouldEqual 5

      val blockChildLambdaScope =
        methodWithBlock.body
          .asInstanceOf[Function.Lambda]
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope
      val blockChildBlockScope =
        methodWithBlock.body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Expression.Block]
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope

      blockChildBlockScope shouldEqual methodWithBlockGraph.rootScope
      blockChildLambdaScope shouldEqual methodWithBlockGraph.rootScope
    }
  }

  "Alias analysis on self" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val addMethod =
      """type Foo
        |    type Foo a b
        |    add x = self.a + x
        |""".stripMargin.preprocessModule.analyse
        .bindings(2)
        .asInstanceOf[definition.Method.Explicit]

    val graph = addMethod
      .unsafeGetMetadata(AliasAnalysis, "Missing aliasing info")
      .unsafeAs[Info.Scope.Root]
      .graph
    val graphLinks = graph.links

    val lambda = addMethod.body.asInstanceOf[Function.Lambda]

    "assign Info.Scope.Root metadata to the method" in {
      val meta = addMethod.getMetadata(AliasAnalysis)
      meta shouldBe defined
    }

    "not add self to the scope" in {
      lambda.arguments.length shouldEqual 2
      lambda.arguments(0).name shouldBe a[Name.Self]
      val topScope = graph.rootScope
      val lambdaScope = lambda
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      topScope shouldEqual lambdaScope
      graphLinks.size shouldEqual 1

      val valueDefId = lambda
        .arguments(1)
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      lambda.body shouldBe an[Application.Prefix]
      val app = lambda.body.asInstanceOf[Application.Prefix]
      val valueUseId = app
        .arguments(1)
        .value
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      // No link between self.x and self
      graphLinks shouldEqual Set(Link(valueUseId, 1, valueDefId))
    }

    "add self as a definition" in {
      lambda.arguments.length shouldEqual 2
      lambda.arguments(0).name shouldBe a[Name.Self]
      val lambdaScope = lambda
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      lambdaScope.allDefinitions.length shouldBe 2
      val defSymbols = lambdaScope.allDefinitions
        .map(definition => definition.symbol)
      defSymbols should equal(List("self", "x"))
    }
  }

  "Alias analysis on conversion methods" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val conversionMethod =
      """Bar.from (that : Foo) =
        |    Bar that.get_thing meh
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method.Conversion]

    val graph = conversionMethod
      .unsafeGetMetadata(AliasAnalysis, "Missing aliasing info")
      .unsafeAs[Info.Scope.Root]
      .graph
    val graphLinks = graph.links

    val lambda     = conversionMethod.body.asInstanceOf[Function.Lambda]
    val lambdaBody = lambda.body.asInstanceOf[Expression.Block]
    val app        = lambdaBody.returnValue.asInstanceOf[Application.Prefix]

    "assign Info.Scope.Root metatata to the method" in {
      val meta = conversionMethod.getMetadata(AliasAnalysis)

      meta shouldBe defined
      meta.get shouldBe an[Info.Scope.Root]
    }

    "assign Info.Scope.Child to all child scopes" in {
      lambda.getMetadata(AliasAnalysis).get shouldBe an[Info.Scope.Child]
      lambdaBody.getMetadata(AliasAnalysis).get shouldBe an[Info.Scope.Child]
    }

    "not allocate additional scopes unnecessarily" in {
      graph.nesting shouldEqual 3
      graph.numScopes shouldEqual 4

      val topScope = graph.rootScope
      val lambdaScope = lambda
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope
      val blockScope = lambdaBody
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      topScope shouldEqual lambdaScope
      lambdaScope shouldEqual blockScope
    }

    "allocate new scopes where necessary" in {
      val topScope = graph.rootScope
      val arg1Scope = app.arguments.head
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope
      val arg2Scope = app
        .arguments(1)
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope

      topScope.childScopes should contain(arg1Scope)
      topScope.childScopes should contain(arg2Scope)
    }

    "assign Info.Occurrence to definitions and usages of symbols" in {
      lambda.arguments.foreach(arg =>
        arg.getMetadata(AliasAnalysis).get.as[Info.Occurrence] shouldBe defined
      )
      val firstAppArg =
        app.arguments.head.value.asInstanceOf[Application.Prefix]
      val innerAppArg = firstAppArg.arguments.head.value
      innerAppArg
        .getMetadata(AliasAnalysis)
        .get
        .as[Info.Occurrence] shouldBe defined
    }

    "create the correct usage links for resolvable entities" in {
      val valueDefId = lambda
        .arguments(1)
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id
      val valueUseId = app.arguments.head.value
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .value
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      graphLinks should contain(Link(valueUseId, 2, valueDefId))
    }

    "not resolve links for unknown symbols" in {
      val unknownHereId = app
        .arguments(1)
        .value
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      graph.linksFor(unknownHereId) shouldBe empty
      graph.getOccurrence(unknownHereId).get shouldBe an[Occurrence.Use]
    }
  }

  "Alias analysis on case expressions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val methodWithCase =
      """
        |List.sum = a -> case a of
        |    Cons a b      -> a + b
        |    Nil           -> 0
        |    num : Integer -> process num Integer
        |    _             -> 0
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]
    val lambda    = methodWithCase.body.asInstanceOf[Function.Lambda]
    val caseBlock = lambda.body.asInstanceOf[Expression.Block]
    val scrutBinding =
      caseBlock.expressions.head.asInstanceOf[Expression.Binding]
    val scrutBindingExpr = scrutBinding.expression.asInstanceOf[Name.Literal]
    val caseExpr         = caseBlock.returnValue.asInstanceOf[Case.Expr]

    val graph = methodWithCase
      .getMetadata(AliasAnalysis)
      .get
      .as[Info.Scope.Root]
      .get
      .graph

    "expose the scrutinee in the parent scope" in {
      val scrutBindingId = scrutBinding
        .unsafeGetMetadata(AliasAnalysis, "")
        .unsafeAs[Info.Occurrence]
        .id
      graph.rootScope.getOccurrence(scrutBindingId) shouldBe defined

      val scrutineeId = caseExpr.scrutinee
        .getMetadata(AliasAnalysis)
        .get
        .as[Info.Occurrence]
        .get
        .id
      graph.rootScope.getOccurrence(scrutineeId) shouldBe defined

      graph.links should contain(Link(scrutineeId, 0, scrutBindingId))

      val scrutBindingExprId = scrutBindingExpr
        .unsafeGetMetadata(AliasAnalysis, "")
        .unsafeAs[Info.Occurrence]
        .id
      graph.rootScope.getOccurrence(scrutBindingExprId) shouldBe defined

      val aDefId = lambda
        .arguments(1)
        .getMetadata(AliasAnalysis)
        .get
        .as[Info.Occurrence]
        .get
        .id
      graph.rootScope.getOccurrence(aDefId) shouldBe defined

      graph.links should contain(Link(scrutBindingExprId, 0, aDefId))
    }

    "create child scopes for the branch function" in {
      val consBranchScope = caseExpr.branches.head
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope
      val nilBranchScope =
        caseExpr
          .branches(1)
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope
      val tpeBranchScope =
        caseExpr
          .branches(2)
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope
      val fallbackBranchScope =
        caseExpr
          .branches(3)
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Scope.Child]
          .scope

      val rootScope = graph.rootScope

      rootScope.childScopes should contain(consBranchScope)
      rootScope.childScopes should contain(nilBranchScope)
      rootScope.childScopes should contain(tpeBranchScope)
      rootScope.childScopes should contain(fallbackBranchScope)
    }

    "cons branch scope should have argument definitions" in {
      val consBranchScope = caseExpr.branches.head
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Scope.Child]
        .scope
      consBranchScope.allDefinitions.length shouldBe 2

      val defSymbols = consBranchScope.allDefinitions
        .map(definition => definition.symbol)
      defSymbols should equal(List("a", "b"))
    }

    "correctly link to pattern variables" in {
      val consBranch = caseExpr.branches.head
      val pattern    = consBranch.pattern.asInstanceOf[Pattern.Constructor]
      val body       = consBranch.expression.asInstanceOf[Application.Prefix]

      val consBranchADef = pattern.fields.head
        .asInstanceOf[Pattern.Name]
        .name
      val consBranchADefId =
        consBranchADef
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id

      val consBranchBDef = pattern.fields(1).asInstanceOf[Pattern.Name].name
      val consBranchBDefId = consBranchBDef
        .getMetadata(AliasAnalysis)
        .get
        .unsafeAs[Info.Occurrence]
        .id

      val aUse = body.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val aUseId =
        aUse.getMetadata(AliasAnalysis).get.unsafeAs[Info.Occurrence].id

      val bUse = body
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val bUseId =
        bUse.getMetadata(AliasAnalysis).get.unsafeAs[Info.Occurrence].id

      graph.links should contain(Link(aUseId, 1, consBranchADefId))
      graph.links should contain(Link(bUseId, 1, consBranchBDefId))
    }

    "correctly link to pattern variables in type patterns" in {
      val tpeBranch = caseExpr.branches(2)
      val pattern   = tpeBranch.pattern.asInstanceOf[Pattern.Type]
      val body      = tpeBranch.expression.asInstanceOf[Application.Prefix]

      val tpeBranchNameDef   = pattern.name
      val tpeTpeBranchTpeDef = pattern.tpe
      val tpeBranchNameDefId =
        tpeBranchNameDef
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id
      val tpeBranchTpeDefId =
        tpeTpeBranchTpeDef
          .getMetadata(AliasAnalysis)
          .get
          .unsafeAs[Info.Occurrence]
          .id

      val numUse = body
        .arguments(0)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val numUseId =
        numUse.getMetadata(AliasAnalysis).get.unsafeAs[Info.Occurrence].id

      val integerUse = body
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val integerUseId =
        integerUse.getMetadata(AliasAnalysis).get.unsafeAs[Info.Occurrence].id

      graph.links should contain(Link(numUseId, 1, tpeBranchNameDefId))
      graph.links should not contain (Link(integerUseId, 1, tpeBranchTpeDefId))
    }
  }

  "Alias analysis on typeset literals" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val method =
      """
        |main =
        |    { x := 1, b := 2 }
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]

    val block = method.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    val blockScope =
      block.unsafeGetMetadata(AliasAnalysis, "").unsafeAs[Info.Scope.Child]

    "create a new scope for the literal" in {
      if (!block.returnValue.isInstanceOf[errors.Syntax]) {
        val literal =
          block.returnValue.asInstanceOf[Application.Typeset]
        val literalScope =
          literal
            .unsafeGetMetadata(AliasAnalysis, "")
            .unsafeAs[Info.Scope.Child]
        blockScope.scope.childScopes should contain(literalScope.scope)
      }
    }
  }

  "Redefinitions" should {
    "be caught for bindings" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val method =
        """
          |main =
          |    a = 1 + 1
          |    b = a * 10
          |    a = b + 1
          |
          |    IO.println a
          |""".stripMargin.preprocessModule.analyse.bindings.head
          .asInstanceOf[definition.Method]
      val block =
        method.body
          .asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Expression.Block]

      block.expressions(2) shouldBe an[errors.Redefined.Binding]
      atLeast(1, block.expressions) shouldBe an[errors.Redefined.Binding]
    }
  }
}

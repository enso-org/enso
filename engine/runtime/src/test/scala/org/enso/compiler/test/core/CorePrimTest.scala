package org.enso.compiler.test.core

import org.enso.core.CoreGraph.DefinitionGen.Node.LocationVal
import org.enso.graph.{Graph => PrimGraph}
import org.enso.syntax.text.AST
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** This file tests the primitive, low-level operations on core.
  *
  * It does _not_ utilise the high-level API, and instead works directly with
  * the defined graph primitives.
  *
  * PLEASE NOTE: Many of these tests will be removed once the smart constructors
  * exist.
  */
class CorePrimTest extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  // === Test Setup ===========================================================
  import org.enso.core.CoreGraph.DefinitionGen.Link.Shape._
  import org.enso.core.CoreGraph.DefinitionGen.Node.Location._
  import org.enso.core.CoreGraph.DefinitionGen.Node.ParentLinks._
  import org.enso.core.CoreGraph.DefinitionGen.Node.Shape._
  import org.enso.core.CoreGraph.DefinitionGen._

  // Reassignable mutable fixture elements
  implicit var graph: PrimGraph.GraphData[CoreGraph] = _
  implicit var literalStorage: LiteralStorage        = _
  implicit var parentStorage: ParentStorage          = _
  implicit var nameStorage: NameStorage              = _
  implicit var astStorage: AstStorage                = _

  override def beforeEach(): Unit = {
    graph          = PrimGraph[CoreGraph]()
    literalStorage = LiteralStorage()
    parentStorage  = ParentStorage()
    nameStorage    = NameStorage()
    astStorage     = AstStorage()
  }

  // === Tests for Links ======================================================

  val link = "A link"

  link should "only be equal to itself" in {
    val l1: Link[CoreGraph] = graph.addLink()
    val l2: Link[CoreGraph] = graph.addLink()

    l1 shouldEqual l1
    l1 should not equal l2
  }

  link should "have a source and a target" in {
    val l1: Link[CoreGraph]       = graph.addLink()
    val srcNode: Node[CoreGraph]  = graph.addNode()
    val destNode: Node[CoreGraph] = graph.addNode()

    l1.source = srcNode
    l1.target = destNode

    val expectedShape = Link.ShapeVal(srcNode, destNode)

    l1.shape shouldEqual expectedShape
  }

  // === Tests for Nodes ======================================================

  val node      = "A node"
  val nodeShape = "A node's shape"

  node should "only be equal to itself" in {
    val n1 = graph.addNode()
    val n2 = graph.addNode()

    n1 shouldEqual n1
    n1 should not equal n2
  }

  node should "contain source location information" in {
    val n1 = graph.addNode()

    n1.sourceStart = 302
    n1.sourceEnd   = 364

    val expectedLocation = Node.LocationVal(302, 364)

    n1.location shouldEqual expectedLocation
  }

  node should "be able to have multiple parent edges" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    l1.target = n1
    l2.target = n1
    l3.target = n1

    val parentIndices: Vector[Int] = Vector(l1.ix, l2.ix, l3.ix)
    n1.parents = parentIndices

    n1.parents shouldEqual parentIndices
    n1.parents.length shouldEqual 3
  }

  node should "be able to take on multiple shapes" in {
    val n1 = graph.addNode()

    Node.setShape[Empty](n1)

    n1 match {
      case Empty.any(_) =>
        Node.setShape[AtomDef](n1)

        n1 match {
          case AtomDef.any(_) => succeed
          case _              => fail
        }
      case _ => fail
    }
  }

  node should "be able to be constructed without clobbering its fields" in {
    val emptyLink = graph.addLink()
    val nilLink   = graph.addLink()
    val consNode  = Node.addRefined[MetaList]

    consNode.head     = emptyLink
    consNode.tail     = nilLink
    consNode.location = LocationVal[CoreGraph](20, 30)
    consNode.parents  = Vector()

    // Intentional re-assignment in reverse order to check for clobbering
    consNode.tail = nilLink
    consNode.head = emptyLink

    consNode.head shouldEqual emptyLink
    consNode.tail shouldEqual nilLink
    consNode.sourceStart shouldEqual 20
    consNode.sourceEnd shouldEqual 30
    consNode.parents shouldEqual Vector()

    consNode.wrapped match {
      case MetaList.any(_) => succeed
      case _               => fail
    }
  }

  // === Tests for Node Shapes ================================================

  nodeShape should "be able to be empty" in {
    val n1 = graph.addNode()

    Node.setShape[Empty](n1)

    val isEmpty = n1 match {
      case Empty.any(_) => true
      case _            => false
    }

    isEmpty shouldEqual true
  }

  nodeShape should "be able to represent a list cons cell" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[MetaList](n1)

    n1 match {
      case MetaList.any(n1) =>
        n1.head = l1
        n1.tail = l2

        n1.metaList shouldEqual MetaListVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent boolean true" in {
    val n1 = graph.addNode()

    Node.setShape[MetaNil](n1)

    n1 match {
      case MetaNil.any(_) => succeed
      case _              => fail
    }
  }

  nodeShape should "be able to represent a nil cell" in {
    val n1 = graph.addNode()

    Node.setShape[MetaTrue](n1)

    n1 match {
      case MetaTrue.any(_) => succeed
      case _               => fail
    }
  }

  nodeShape should "be able to represent boolean false" in {
    val n1 = graph.addNode()

    Node.setShape[MetaFalse](n1)

    n1 match {
      case MetaFalse.any(_) => succeed
      case _                => fail
    }
  }

  nodeShape should "be able to represent a numeric literal" in {
    val n1 = graph.addNode()

    Node.setShape[NumericLiteral](n1)

    n1 match {
      case NumericLiteral.any(n1) =>
        val testText = "1e-1"
        n1.number = testText

        n1.numericLiteral shouldEqual NumericLiteralVal(testText)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a text literal" in {
    val n1 = graph.addNode()

    Node.setShape[TextLiteral](n1)

    n1 match {
      case TextLiteral.any(n1) =>
        val testText = "Lorem ipsum"
        n1.text = testText

        n1.textLiteral shouldEqual TextLiteralVal(testText)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a foreign code literal" in {
    val n1 = graph.addNode()

    Node.setShape[ForeignCodeLiteral](n1)

    n1 match {
      case ForeignCodeLiteral.any(n1) =>
        val testText = "lambda x: x + 1"
        n1.code = testText

        n1.foreignCodeLiteral shouldEqual ForeignCodeLiteralVal(testText)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a name" in {
    val n1 = graph.addNode()

    Node.setShape[Name](n1)

    n1 match {
      case Name.any(n1) =>
        val testName = "Name"
        n1.nameLiteral = testName

        n1.name shouldEqual NameVal(testName)
      case _ => fail
    }
  }

  nodeShape should "be able to represent `this`" in {
    val n1 = graph.addNode()

    Node.setShape[ThisName](n1)

    n1 match {
      case ThisName.any(_) => succeed
      case _               => fail
    }
  }

  nodeShape should "be able to represent `here`" in {
    val n1 = graph.addNode()

    Node.setShape[HereName](n1)

    n1 match {
      case HereName.any(_) => succeed
      case _               => fail
    }
  }

  nodeShape should "be able to represent a module" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[ModuleDef](n1)

    n1 match {
      case ModuleDef.any(n1) =>
        n1.name        = l1
        n1.imports     = l2
        n1.definitions = l3

        n1.moduleDef shouldEqual ModuleDefVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent an import" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[Import](n1)

    n1 match {
      case Import.any(n1) =>
        n1.segments = l1

        n1.`import` shouldEqual ImportVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a lop-level binding" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TopLevelBinding](n1)

    n1 match {
      case TopLevelBinding.any(n1) =>
        n1.module  = l1
        n1.binding = l2

        n1.topLevelBinding shouldEqual TopLevelBindingVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent an atom definition" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[AtomDef](n1)

    n1 match {
      case AtomDef.any(n1) =>
        n1.name = l1
        n1.args = l2

        n1.atomDef shouldEqual AtomDefVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a complex type definition" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[TypeDef](n1)

    n1 match {
      case TypeDef.any(n1) =>
        n1.name       = l1
        n1.typeParams = l2
        n1.body       = l3

        n1.typeDef shouldEqual TypeDefVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a type signature" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypeAscription](n1)

    n1 match {
      case TypeAscription.any(n1) =>
        n1.typed = l1
        n1.sig   = l2

        n1.typeAscription shouldEqual TypeAscriptionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent monadic context ascription" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[ContextAscription](n1)

    n1 match {
      case ContextAscription.any(n1) =>
        n1.typed   = l1
        n1.context = l2

        n1.contextAscription shouldEqual ContextAscriptionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a typeset member" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[TypesetMember](n1)

    n1 match {
      case TypesetMember.any(n1) =>
        n1.label      = l1
        n1.memberType = l2
        n1.value      = l3

        n1.typesetMember shouldEqual TypesetMemberVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the subsumption judgement" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetSubsumption](n1)

    n1 match {
      case TypesetSubsumption.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetSubsumption shouldEqual TypesetSubsumptionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the eqquality judgement" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetEquality](n1)

    n1 match {
      case TypesetEquality.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetEquality shouldEqual TypesetEqualityVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the concatenation operator" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetConcat](n1)

    n1 match {
      case TypesetConcat.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetConcat shouldEqual TypesetConcatVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the union operator" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetUnion](n1)

    n1 match {
      case TypesetUnion.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetUnion shouldEqual TypesetUnionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the intersection operator" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetIntersection](n1)

    n1 match {
      case TypesetIntersection.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetIntersection shouldEqual TypesetIntersectionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the subtraction operator" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[TypesetSubtraction](n1)

    n1 match {
      case TypesetSubtraction.any(n1) =>
        n1.left  = l1
        n1.right = l2

        n1.typesetSubtraction shouldEqual TypesetSubtractionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a lambda" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[Lambda](n1)

    n1 match {
      case Lambda.any(n1) =>
        n1.arg  = l1
        n1.body = l2

        n1.lambda shouldEqual LambdaVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a function definition" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[FunctionDef](n1)

    n1 match {
      case FunctionDef.any(n1) =>
        n1.name = l1
        n1.args = l2
        n1.body = l3

        n1.functionDef shouldEqual FunctionDefVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent a method definition" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[MethodDef](n1)

    n1 match {
      case MethodDef.any(n1) =>
        n1.targetPath = l1
        n1.name       = l2
        n1.function   = l3

        n1.methodDef shouldEqual MethodDefVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent an ignored argument" in {
    val n1 = graph.addNode()

    Node.setShape[IgnoredArgument](n1)

    n1 match {
      case IgnoredArgument.any(_) => succeed
      case _                      => fail
    }
  }

  nodeShape should "be able to represent a definition-site argument" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[DefinitionArgument](n1)

    n1 match {
      case DefinitionArgument.any(n1) =>
        n1.name      = l1
        n1.suspended = l2
        n1.default   = l3

        n1.definitionArgument shouldEqual DefinitionArgumentVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent prefix function application" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[Application](n1)

    n1 match {
      case Application.any(n1) =>
        n1.function = l1
        n1.argument = l2

        n1.application shouldEqual ApplicationVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent infix function application" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()
    val l3 = graph.addLink()

    Node.setShape[InfixApplication](n1)

    n1 match {
      case InfixApplication.any(n1) =>
        n1.left     = l1
        n1.operator = l2
        n1.right    = l3

        n1.infixApplication shouldEqual InfixApplicationVal(l1, l2, l3)
      case _ => fail
    }
  }

  nodeShape should "be able to represent left sections" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[LeftSection](n1)

    n1 match {
      case LeftSection.any(n1) =>
        n1.arg      = l1
        n1.operator = l2

        n1.leftSection shouldEqual LeftSectionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent right sections" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[RightSection](n1)

    n1 match {
      case RightSection.any(n1) =>
        n1.operator = l1
        n1.arg      = l2

        n1.rightSection shouldEqual RightSectionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent centre sections" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[CentreSection](n1)

    n1 match {
      case CentreSection.any(n1) =>
        n1.operator = l1

        n1.centreSection shouldEqual CentreSectionVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent forced terms" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[ForcedTerm](n1)

    n1 match {
      case ForcedTerm.any(n1) =>
        n1.expression = l1

        n1.forcedTerm shouldEqual ForcedTermVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent _ arguments" in {
    val n1 = graph.addNode()

    Node.setShape[LambdaShorthandArgument](n1)

    n1 match {
      case LambdaShorthandArgument.any(_) => succeed
      case _                              => fail
    }
  }

  nodeShape should "be able to represent call site arguments" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[CallSiteArgument](n1)

    n1 match {
      case CallSiteArgument.any(n1) =>
        n1.expression = l1
        n1.name       = l2

        n1.callSiteArgument shouldEqual CallSiteArgumentVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent the defaults suspension operator" in {
    val n1 = graph.addNode()

    Node.setShape[SuspendDefaultsOperator](n1)

    n1 match {
      case SuspendDefaultsOperator.any(_) => succeed
      case _                              => fail
    }
  }

  nodeShape should "be able to represent block expressions" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[Block](n1)

    n1 match {
      case Block.any(n1) =>
        n1.expressions = l1
        n1.returnVal   = l2

        n1.block shouldEqual BlockVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent bindings" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[Binding](n1)

    n1 match {
      case Binding.any(n1) =>
        n1.name       = l1
        n1.expression = l2

        n1.binding shouldEqual BindingVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent case expressions" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[CaseExpr](n1)

    n1 match {
      case CaseExpr.any(n1) =>
        n1.scrutinee = l1
        n1.branches  = l2

        n1.caseExpr shouldEqual CaseExprVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent case branches" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[CaseBranch](n1)

    n1 match {
      case CaseBranch.any(n1) =>
        n1.pattern    = l1
        n1.expression = l2

        n1.caseBranch shouldEqual CaseBranchVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent structural patterns" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[StructuralPattern](n1)

    n1 match {
      case StructuralPattern.any(n1) =>
        n1.matchExpression = l1

        n1.structuralPattern shouldEqual StructuralPatternVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent type-based patterns" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[TypePattern](n1)

    n1 match {
      case TypePattern.any(n1) =>
        n1.matchExpression = l1

        n1.typePattern shouldEqual TypePatternVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent named type-based patterns" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[NamedPattern](n1)

    n1 match {
      case NamedPattern.any(n1) =>
        n1.matchExpression = l1

        n1.namedPattern shouldEqual NamedPatternVal(l1)
      case _ => fail
    }
  }

  nodeShape should "be able to represent fallback patterns" in {
    val n1 = graph.addNode()

    Node.setShape[FallbackPattern](n1)

    n1 match {
      case FallbackPattern.any(_) => succeed
      case _                      => fail
    }
  }

  nodeShape should "be able to represent doc comments" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[DocComment](n1)

    n1 match {
      case DocComment.any(n1) =>
        n1.commented = l1
        n1.doc       = l2

        n1.docComment shouldEqual DocCommentVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent foreign code segments" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()
    val l2 = graph.addLink()

    Node.setShape[ForeignDefinition](n1)

    n1 match {
      case ForeignDefinition.any(n1) =>
        n1.language = l1
        n1.code     = l2

        n1.foreignDefinition shouldEqual ForeignDefinitionVal(l1, l2)
      case _ => fail
    }
  }

  nodeShape should "be able to represent syntax errors" in {
    val n1  = graph.addNode()
    val ast = AST.Blank()

    Node.setShape[SyntaxError](n1)

    n1 match {
      case SyntaxError.any(n1) =>
        n1.errorAst = ast

        n1.syntaxError shouldEqual SyntaxErrorVal(ast)
      case _ => fail
    }
  }

  nodeShape should "be able to represent construction errors" in {
    val n1 = graph.addNode()
    val l1 = graph.addLink()

    Node.setShape[ConstructionError](n1)

    n1 match {
      case ConstructionError.any(n1) =>
        n1.erroneousCore = l1

        n1.constructionError shouldEqual ConstructionErrorVal(l1)
    }
  }
}

package org.enso.compiler.test.core

import cats.data.NonEmptyList
import org.enso.compiler.core.Core
import org.enso.compiler.core.Core.Node.Utility
import org.enso.compiler.test.CompilerTest
import org.enso.core.CoreGraph.DefinitionGen.Node.{Shape => NodeShape}
import org.enso.core.CoreGraph.{DefinitionGen => CoreDef}
import org.enso.graph.{Graph => PrimGraph}

class CoreTest extends CompilerTest {

  // === Test Setup ===========================================================

  import Core._
  import CoreDef.Link.Shape._
  import CoreDef.Node.ParentLinks._
  import CoreDef.Node.Shape._
  import PrimGraph.Component.Refined._
  import PrimGraph.VariantCast

  // === Useful Constants =====================================================

  val constantLocationStart = 201
  val constantLocationEnd   = 1337
  val dummyLocation: Core.Location =
    CoreDef.Node.LocationVal(constantLocationStart, constantLocationEnd)

  // === More Complex Graph Shape Tests =======================================

  // TODO [AA] Once deletion and replacement functions exist, expand these tests
  //  to check that the shapes behave properly under usage of such functions.

  "Diamonds constructed on the graph" should {
    implicit val core: Core = new Core()

    val fnName       = Node.New.Name("foo", dummyLocation)
    val binding1Name = Node.New.Name("a", dummyLocation)
    val binding1     = Node.New.Binding(binding1Name, fnName, dummyLocation)
    val binding2Name = Node.New.Name("b", dummyLocation)
    val binding2     = Node.New.Binding(binding2Name, fnName, dummyLocation)
    val bindingsList = Utility.ListOps.from(binding1)

    val block =
      Node.New.Block(bindingsList, binding2, dummyLocation).getOrElse(fail())

    "have multiple parents for the node at the bottom of the diamond" in {
      fnName.parents.size shouldEqual 2
      fnName.parents should contain(binding1.expression.ix)
      fnName.parents should contain(binding2.expression.ix)
    }

    "ensure that traversals through both paths reach the same place" in {
      val fnNameViaLeftPath = block.expressions.target
        .unsafeAs[NodeShape.MetaList]
        .head
        .target
        .unsafeAs[NodeShape.Binding]
        .expression
        .target

      val fnNameViaRightPath =
        block.returnVal.target.unsafeAs[NodeShape.Binding].expression.target

      fnNameViaLeftPath shouldEqual fnNameViaRightPath
    }
  }

  "Multi-level trees constructed on the graph" should {
    implicit val core: Core = new Core()

    /* Builds the following tree for purposes of the test
     *
     *    1
     *    | \
     *    2  3
     *    | \
     *    4  5
     *       | \
     *       6  7
     */

    val node7 = Node.New.Empty()
    val node6 = Node.New.Empty()
    val node5 = Node.New.LeftSection(node6, node7, dummyLocation)
    val node4 = Node.New.Empty()
    val node3 = Node.New.Empty()
    val node2 = Node.New.LeftSection(node4, node5, dummyLocation)
    val node1 = Node.New.LeftSection(node2, node3, dummyLocation)

    "allow walking to all the leaf nodes" in {
      node1.arg.target
        .unsafeAs[NodeShape.LeftSection]
        .arg
        .target shouldEqual node4

      node1.operator.target shouldEqual node3

      node1.arg.target
        .unsafeAs[NodeShape.LeftSection]
        .operator
        .target
        .unsafeAs[NodeShape.LeftSection]
        .arg
        .target shouldEqual node6

      node1.arg.target
        .unsafeAs[NodeShape.LeftSection]
        .operator
        .target
        .unsafeAs[NodeShape.LeftSection]
        .operator
        .target shouldEqual node7
    }

    "allow walking from leaf to parent via parent links" in {
      val node5handle =
        core.graph.componentRefFromIndex[Links](node7.parents.head).source
      val node2handle =
        core.graph
          .componentRefFromIndex[Links](node5handle.parents.head)
          .source
      val node1handle =
        core.graph
          .componentRefFromIndex[Links](node2handle.parents.head)
          .source

      node1handle shouldEqual node1
    }
  }

  "Cycles on the graph" should {
    implicit val core: Core = new Core()

    val empty = Node.New.Empty()

    val tempNil = Node.New.MetaNil()
    val cons4   = Node.New.MetaList(empty, tempNil).getOrElse(fail())
    val cons3   = Node.New.MetaList(empty, cons4).getOrElse(fail())
    val cons2   = Node.New.MetaList(empty, cons3).getOrElse(fail())
    val cons1   = Node.New.MetaList(empty, cons2).getOrElse(fail())

    // Link the nodes in a loop
    val loopLink = Link.New.Connected(cons4, cons1)
    cons4.tail = loopLink

    "allow correct traversal" in {
      cons1.tail.target
        .unsafeAs[NodeShape.MetaList] // cons2
        .tail
        .target
        .unsafeAs[NodeShape.MetaList] // cons3
        .tail
        .target
        .unsafeAs[NodeShape.MetaList] // cons4
        .tail
        .target shouldEqual cons1
    }

    "allow reverse traversal" in {
      val cons4ref =
        core.graph.componentRefFromIndex[Links](cons1.parents.head).target
      val cons3ref =
        core.graph.componentRefFromIndex[Links](cons4ref.parents.head).target
      val cons2ref =
        core.graph.componentRefFromIndex[Links](cons3ref.parents.head).target
      val cons1ref =
        core.graph.componentRefFromIndex[Links](cons2ref.parents.head).target

      cons1ref shouldEqual cons1
    }
  }

  "Linked lists on the graph" should {
    implicit val core: Core = new Core()

    val myBindingName = Node.New.Name("a", dummyLocation)
    val myFnName      = Node.New.Name("function", dummyLocation)
    val myBinding     = Node.New.Binding(myBindingName, myFnName, dummyLocation)

    val list = Utility.ListOps.from(
      NonEmptyList(
        myBinding.wrapped,
        List(
          myBindingName.wrapped,
          myFnName.wrapped
        )
      )
    )

    "should allow finding of their end" in {
      list.tail.target
        .unsafeAs[NodeShape.MetaList] // cons2
        .tail
        .target
        .unsafeAs[NodeShape.MetaList] // cons3
        .tail
        .target shouldEqual Utility.ListOps.end(list).get
    }

    "be able to contain complex structures" in {
      val bindingRef =
        Utility.ListOps.at(list, 0).getOrElse(fail()).unsafeAs[NodeShape.Binding]

      bindingRef shouldEqual myBinding

      bindingRef.name shouldEqual myBindingName
      bindingRef.expression shouldEqual myFnName
    }

    "allow forward traversal" in {
      list.tail.target
        .unsafeAs[NodeShape.MetaList]
        .head
        .target shouldEqual myBindingName
    }

    "allow reverse traversal via parent links" in {
      val listEnd = Utility.ListOps.end(list).get
      listEnd.is[NodeShape.MetaNil] shouldEqual true

      val cons3ref =
        core.graph
          .componentRefFromIndex[Links](listEnd.parents.head)
          .source
          .as[NodeShape.MetaList]
          .getOrElse(fail())
      cons3ref.head.target.is[NodeShape.Name] shouldEqual true
      cons3ref.head.target shouldEqual myFnName

      val cons2ref = core.graph
        .componentRefFromIndex[Links](cons3ref.parents.head)
        .source
        .unsafeAs[NodeShape.MetaList]
      cons2ref.head.target.is[NodeShape.Name] shouldEqual true
      cons2ref.head.target shouldEqual myBindingName

      val cons1ref = core.graph
        .componentRefFromIndex[Links](cons2ref.parents.head)
        .source
        .unsafeAs[NodeShape.MetaList]
      cons1ref.head.target.is[NodeShape.Binding] shouldEqual true
      cons1ref.head.target shouldEqual myBinding
    }
  }
}

package org.enso.graph

import org.enso.graph.{Graph => PrimGraph}
import org.scalatest.{FlatSpec, Matchers}
import org.enso.graph.GraphTestDefinition._

/** This file contains tests for the graph library. */
class GraphTest extends FlatSpec with Matchers {

  // ==========================================================================
  // === Example Graph Usage ==================================================
  // ==========================================================================

  import GraphImpl._
  import GraphImpl.Node.ParentLink._
  import GraphImpl.Node.Location._
  import GraphImpl.Edge.Shape._

  implicit val graph: PrimGraph.GraphData[Graph] = PrimGraph[Graph]();
  implicit val stringStorage: StrStorage         = StrStorage()
  implicit val backrefStorage: BackrefStorage    = BackrefStorage()

  val n1: Node[Graph] = graph.addNode()
  val n2: Node[Graph] = graph.addNode()
  val n3: Node[Graph] = graph.addNode()

  val e1: Edge[Graph] = graph.addEdge()
  e1.source = n1
  e1.target = n2

  n1.parent = e1
  n2.parent = e1
  n3.parent = e1

  // Change `n1` to be `App`
  graph.unsafeSetVariantCase[
    Nodes,
    GraphImpl.Node.Shape,
    GraphImpl.Node.Shape.App
  ](n1)

  // Change `n2` to be `Name`
  graph.unsafeSetVariantCase[
    Nodes,
    GraphImpl.Node.Shape,
    GraphImpl.Node.Shape.Name
  ](n2)

  // Change `n3` to be `Nul`
  graph.unsafeSetVariantCase[
    Nodes,
    GraphImpl.Node.Shape,
    GraphImpl.Node.Shape.Nul
  ](n3)

  // ==========================================================================
  // === Tests ================================================================
  // ==========================================================================

  "Component fields" should "be able to be accessed by their types" in {
    n1.line   = 10
    n1.column = 5

    n1.line shouldEqual 10
    n1.column shouldEqual 5

    n1.location = Node.LocationVal[Graph](1, 2)

    n1.line shouldEqual 1
    n1.column shouldEqual 2
  }

  "Opaque types" should "be accessed successfully" in {
    val nameStr = "TestName"
    val n2Refined = n2 match {
      case GraphImpl.Node.Shape.Name.any(n2) => n2
    }

    n2Refined.str = nameStr
    n2Refined.str shouldEqual nameStr
  }

  "Matching on variants" should "work properly" in {
    val typeResult = n1 match {
      case GraphImpl.Node.Shape.Nul.any(n @ _)  => "Null"
      case GraphImpl.Node.Shape.App.any(n1 @ _) => "App1"
      case GraphImpl.Node.Shape.App(_, _)       => "App2"
    }

    typeResult shouldEqual "App1"
  }

  "Matching on variants" should "refine the variant type" in {
    val refinedResult = n1 match {
      case GraphImpl.Node.Shape.App.any(n1) => n1.fn
    }

    refinedResult shouldEqual 1
  }

  "Component fields" can "be accessed properly" in {
    e1.source shouldEqual n1
    e1.target shouldEqual n2
  }

  "The graph" should "be mutable" in {
    val e2: Edge[Graph] = graph.addEdge()
    e2.source = n1
    e2.target = n2

    e2.source shouldEqual n1

    e2.source = n3

    e2.source shouldEqual n3
  }
}

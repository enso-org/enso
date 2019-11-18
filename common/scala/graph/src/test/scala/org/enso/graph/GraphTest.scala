package org.enso.graph

import org.enso.graph.Graph.Component
import org.enso.graph.definition.Macro.{component, field}
import org.scalatest.{FlatSpec, Matchers}
import shapeless.{::, HNil}

class GraphTest extends FlatSpec with Matchers {
  object GraphImpl {

    // ========================================================================
    // === Component Definitions ==============================================
    // ========================================================================

    // === Node ===
    @component case class Nodes() { type Node[G <: Graph] }

    // === Edge ===
    @component case class Edges() { type Edge[G <: Graph] }

    // ========================================================================
    // === Component Field Definitions ========================================
    // ========================================================================

    object Node {

      // === Node Shape ===
      @field object Shape {
        case class Null()
        case class App[G <: Graph](fn: Edge[G], argTest: Edge[G])
      }

      // === ParentLink ===
      @field case class ParentLink[G <: Graph](parent: Edge[G])
    }

    object Edge {

      // === Edge Shape ===
      @field case class Shape[G <: Graph](source: Node[G], target: Node[G])
    }

    // ========================================================================
    // === Example Graph Implementation =======================================
    // ========================================================================

    case class MyGraph() extends Graph

    implicit def components = new Graph.Component.List[MyGraph] {
      type Out = Nodes :: Edges :: HNil
    }

    implicit def nodeFields = new Graph.Component.Field.List[MyGraph, Nodes] {
      type Out = Node.Shape :: Node.ParentLink :: HNil
    }

    implicit def edgeFields = new Graph.Component.Field.List[MyGraph, Edges] {
      type Out = Edge.Shape :: HNil
    }
  }

  // ==========================================================================
  // === Example Graph Usage ==================================================
  // ==========================================================================

  import GraphImpl.Edge.Shape._
  import GraphImpl.Node.ParentLink._
  import GraphImpl.Node.Shape.App._
  import GraphImpl._

  implicit val graph = Graph[GraphImpl.MyGraph]();

  val n1: Node[MyGraph] = graph.addNode()
  val n2: Node[MyGraph] = graph.addNode()
  val n3: Node[MyGraph] = graph.addNode()

  val e1: Edge[MyGraph] = graph.addEdge()
  e1.source = n1
  e1.target = n2

  n1.parent = Component.Ref(1)
  n2.parent = Component.Ref(2)
  n3.parent = Component.Ref(3)

  // This is just dirty and very unsafe way of changing `n1` to be App!
  graph.unsafeWriteField[Nodes, GraphImpl.Node.Shape](n1.ix, 0, 1)

  // ==========================================================================
  // === Tests ================================================================
  // ==========================================================================

  "Matching on variants" should "work properly" in {
    val typeResult = n1 match {
      case GraphImpl.Node.Shape.Null.any(n @ _) => "Null"
      case GraphImpl.Node.Shape.App.any(n1 @ _) => "App1"
      case GraphImpl.Node.Shape.App(_, _) => "App2"
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
    val e2: Edge[MyGraph] = graph.addEdge()
    e2.source = n1
    e2.target = n2

    e2.source shouldEqual n1

    e2.source = n3

    e2.source shouldEqual n3
  }
}

package org.enso.graph

import org.enso.graph.definition.Macro.component
import org.scalatest.{FlatSpec, Matchers}
import shapeless.test.illTyped

class ComponentMacroTest extends FlatSpec with Matchers {

  "The `@component` macro" should "define correct components" in {
    "@component case class Nodes() { type Node[G <: Graph] }" should compile
    "@component case class Edges() { type Edge[G <: Graph] }" should compile
  }

  "The `@component` macro child type" should "be accessible" in {
    "@component case class Nodes() { type Node[G <: Graph] }\n type Test = Node[Graph]" should compile
  }

  "The `@component` macro child type" must "error when its type parameter does not subclass graph" in {
    illTyped(
      "@component case class Nodes() { type Node[G] }",
      "The contained type's parameter must be a subtype of Graph"
    )
  }

  "The `@component` child type" must "not allow more than one type parameter" in {
    illTyped(
      "@component case class Nodes() { type Node[G <: Graph, C] }",
      "Your contained type must only have one type parameter"
    )
  }

  "The `@component` macro" should "only allow application to case classes" in {
    illTyped (
      "@component object Test",
      "You must provide a class definition to the @component macro"
    )
    illTyped (
      "@component class Foo()",
      "@component must be applied to a case class"
    )
  }
}

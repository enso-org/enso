package org.enso.graph

import org.enso.graph.definition.Macro.{component, field, opaque}
import org.enso.graph.{Graph => PrimGraph}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class FieldMacroTest extends AnyFlatSpec with Matchers {

  val subject = "The `@field` macro"

  // === Components for Tests =================================================
  @component case class Nodes() { type Node[G <: Graph] }
  @component case class Edges() { type Edge[G <: Graph] }
  @opaque case class Backref[G <: PrimGraph](opaque: Vector[Int])
  @opaque case class Str[G <: PrimGraph](opaque: String)

  // === Tests ================================================================
  subject should "not explicitly depend on the graph type name" in {
    "@field case class Shape[G <: PrimGraph](source: Node[G], target: Node[G])" should compile
  }

  subject should "work with returning the raw storage type" in {
    "@field case class Location[G <: PrimGraph](line: Int, column: Int)" should compile
  }

  subject should "support opaque types using marker trait" in {
    "@field case class Backref[G <: PrimGraph](references: OpaqueData[Vector[Int], BackrefStorage])" should compile
  }

  subject should "work for single fields" in {
    "@field case class Shape[G <: Graph](source: Node[G], target: Node[G])" should compile
  }

  "The `@field` macro" should "work for variant fields" in {
    "@field object Shape {\n  type G = PrimGraph\n  case class Nul()\n  case class App(fn: Edge[G], arg: Edge[G])}" should compile
  }

  "Access to companion objects for fields" should "work as expected" in {
    "@field object Shape {\n  type G = Graph\n  case class Nul()\n  case class App[G <: Graph](fn: Edge[G], argTest: Edge[G])}\nval test = Shape.Nul.any" should compile
  }

  subject should "work properly with opaque types in variants" in {
    "@field object Shape {\n" +
    "  type G = PrimGraph\n" +
    "  case class Nul()\n" +
    "  case class Name(str: OpaqueData[String, StrStorage], linkEdge: Edge[G])\n" +
    "}" should compile
  }

  subject should "not allow application to non case classes" in {
    illTyped(
      "@field class Shape",
      "@field must be applied to a case class or object."
    )
  }

  subject should "not allow application to invalid constructs" in {
    illTyped(
      "@field type foo",
      "The @field macro only operates on case classes."
    )
  }

  subject should "error on variants without branches" in {
    illTyped(
      "@field object Shape{\n  type G = Graph}",
      "A variant must contain at least one case."
    )
  }

  subject should "error on variants that do not define `type G`" in {
    illTyped(
      "@field object Shape{\n  case class Nul()}",
      "You must define a type named `G` in your variant that defines the graph type name."
    )
  }

  subject should "error if a field defines a subfield with a name clash" in {
    illTyped(
      "@field case class Location[G <: PrimGraph](location: Int)",
      "You cannot define a subfield name that clashes with the field name."
    )
  }

  subject should "error if a variant defines a subfield with a name clash" in {
    illTyped(
      "@field object Shape {  type G = PrimGraph\n  case class Foo(shape: Edge[G])}",
      "You cannot define a variant subfield that clashes with either the variant or case name."
    )

    illTyped(
      "@field object Shape {  type G = PrimGraph\n  case class Foo(foo: Edge[G])}",
      "You cannot define a variant subfield that clashes with either the variant or case name."
    )
  }

  subject should "error if a variant case name clashes with the variant name" in {
    illTyped(
      "@field object Shape {  type G = PrimGraph\n  case class Shape(foo: Edge[G])}",
      "A variant case cannot share its name with the variant."
    )
  }
}

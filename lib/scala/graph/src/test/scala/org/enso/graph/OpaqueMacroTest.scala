package org.enso.graph

import org.enso.graph.definition.Macro.opaque
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

import scala.collection.mutable

class OpaqueMacroTest extends AnyFlatSpec with Matchers {
  val subject = "The @opaque macro"

  subject should "define proper opaque maps" in {
    "@opaque case class Backref(opaque: Vector[Int])" should compile
    "@opaque case class String(opaque: String)" should compile
  }

  subject should "error if not passed a class def" in {
    illTyped(
      "@opaque object Foo",
      "You must provide a class definition to the @opaque macro."
    )
  }

  subject should "error if defined with incorrect val members" in {
    illTyped(
      "@opaque case class String()",
      "You must define a constructor member called `opaque` that specifies your opaque type."
    )
  }

  subject should "allow access to its member under the correct name" in {
    @opaque case class Backref(opaque: String)

    val backrefStorage                   = BackrefStorage()
    val member: mutable.Map[Int, String] = backrefStorage.backref

    val testStr = "Foo"
    member(0) = testStr

    member(0) shouldEqual testStr
  }
}

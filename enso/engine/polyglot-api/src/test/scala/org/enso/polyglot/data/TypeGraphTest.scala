package org.enso.polyglot.data

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeGraphTest extends AnyWordSpec with Matchers {

  "The type graph" should {
    "be able to insert links" in {
      val graph = new TypeGraph("Builtins.Main.Any")
      graph.insert("Builtins.Main.Number", "Builtins.Main.Any")
      graph.insert("Builtins.Main.Decimal", "Builtins.Main.Number")
      graph.insert("Builtins.Main.Integer", "Builtins.Main.Number")
    }

    "be able to query direct parents" in {
      val graph = new TypeGraph("Builtins.Main.Any")
      graph.insert("Builtins.Main.Number", "Builtins.Main.Any")
      graph.insert("Builtins.Main.Decimal", "Builtins.Main.Number")
      graph.insert("Builtins.Main.Integer", "Builtins.Main.Number")

      graph.getDirectParents("Builtins.Main.Decimal") shouldEqual Set(
        "Builtins.Main.Number"
      )
      graph.getDirectParents("Builtins.Main.Integer") shouldEqual Set(
        "Builtins.Main.Number"
      )
      graph.getDirectParents("Builtins.Main.Number") shouldEqual Set(
        "Builtins.Main.Any"
      )
      graph.getDirectParents("Builtins.Main.Any") shouldBe empty
    }

    "be able to query all parents" in {
      val graph = new TypeGraph("Builtins.Main.Any")
      graph.insert("Builtins.Main.Number", "Builtins.Main.Any")
      graph.insert("Builtins.Main.Decimal", "Builtins.Main.Number")
      graph.insert("Builtins.Main.Integer", "Builtins.Main.Number")

      graph.getParents("Builtins.Main.Any") shouldEqual List()
      graph.getParents("Builtins.Main.Number") shouldEqual List(
        "Builtins.Main.Any"
      )
      graph.getParents("Builtins.Main.Integer") shouldEqual List(
        "Builtins.Main.Number",
        "Builtins.Main.Any"
      )
      graph.getParents("Builtins.Main.Decimal") shouldEqual List(
        "Builtins.Main.Number",
        "Builtins.Main.Any"
      )
    }

    "have a fallback parent for any typename" in {
      val graph = new TypeGraph("Builtins.Main.Any")
      graph.insert("Builtins.Main.Number", "Builtins.Main.Any")
      graph.insert("Builtins.Main.Decimal", "Builtins.Main.Number")
      graph.insert("Builtins.Main.Integer", "Builtins.Main.Number")

      graph.getParents("My_User_Type") shouldEqual List("Builtins.Main.Any")
      graph.getParents("Standard.Base.Vector") shouldEqual List(
        "Builtins.Main.Any"
      )
    }
  }
}

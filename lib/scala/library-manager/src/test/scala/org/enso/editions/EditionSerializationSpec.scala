package org.enso.editions

import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class EditionSerializationSpec extends AnyWordSpec with Matchers with Inside {
  "EditionSerialization" should {
    "parse an edition that just derives another one" in {
      val parsed = EditionSerialization.parseYamlString(
        """extends: 2021.4
          |""".stripMargin
      )
      inside(parsed) { case Success(edition) =>
        edition.parent should contain("2021.4")
        edition.repositories should be(empty)
        edition.libraries should be(empty)
        edition.preferLocalLibraries should be(empty)
        edition.engineVersion should be(empty)
      }
    }
    "parse a standalone edition" in                                                            {}
    "not parse an edition that does not define the engine version nor attempt to derive it" in {}
  }
}

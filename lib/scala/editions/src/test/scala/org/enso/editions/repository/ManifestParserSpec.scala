package org.enso.editions.repository

import org.enso.editions.EditionName
import org.enso.testkit.EitherValue
import org.enso.yaml.YamlHelper
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ManifestParserSpec extends AnyWordSpec with Matchers with EitherValue {
  "Manifest" should {
    "be parsed from YAML format" in {
      val str =
        """editions:
          |- foo
          |- 2021.4
          |- bar
          |""".stripMargin
      YamlHelper.parseString[Manifest](str).rightValue.editions shouldEqual Seq(
        EditionName("foo"),
        EditionName("2021.4"),
        EditionName("bar")
      )
    }
  }
}

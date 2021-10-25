package org.enso.librarymanager.published.repository

import org.enso.editions.LibraryName
import org.enso.testkit.EitherValue
import org.enso.yaml.YamlHelper
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibraryManifestParserSpec
    extends AnyWordSpec
    with Matchers
    with EitherValue {
  "LibraryManifest" should {
    "be parsed from YAML format" in {
      val str =
        """archives:
          |- main.tgz
          |- tests.tgz
          |dependencies:
          |- Standard.Base
          |- Foo.Bar
          |tag-line: Foo Bar
          |description: Foo bar baz.
          |""".stripMargin
      YamlHelper
        .parseString[LibraryManifest](str)
        .rightValue shouldEqual LibraryManifest(
        archives = Seq("main.tgz", "tests.tgz"),
        dependencies = Seq(
          LibraryName.fromString("Standard.Base").rightValue,
          LibraryName.fromString("Foo.Bar").rightValue
        ),
        tagLine     = Some("Foo Bar"),
        description = Some("Foo bar baz.")
      )
    }

    "require only a minimal set of fields to parse" in {
      val str =
        """archives:
          |- main.tgz
          |""".stripMargin
      YamlHelper
        .parseString[LibraryManifest](str)
        .rightValue shouldEqual LibraryManifest(
        archives     = Seq("main.tgz"),
        dependencies = Seq(),
        tagLine      = None,
        description  = None
      )
    }
  }
}

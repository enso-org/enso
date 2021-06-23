package org.enso.editions

import org.enso.testkit.EitherValue
import org.enso.yaml.YamlHelper
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LibraryNameSpec
    extends AnyWordSpec
    with Matchers
    with EitherValue
    with EitherValues {
  "LibraryName" should {
    "parse and serialize to the same thing" in {
      val str         = "Foo.Bar"
      val libraryName = LibraryName.fromString(str).rightValue
      libraryName.qualifiedName shouldEqual str
      libraryName.name shouldEqual "Bar"
      libraryName.prefix shouldEqual "Foo"

      val yamlParsed = YamlHelper.parseString[LibraryName](str).rightValue
      yamlParsed shouldEqual libraryName
    }

    "fail to parse if there are too many parts" in {
      LibraryName.fromString("A.B.C") shouldEqual Left(
        "`A.B.C` is not a valid library name."
      )
    }
  }
}

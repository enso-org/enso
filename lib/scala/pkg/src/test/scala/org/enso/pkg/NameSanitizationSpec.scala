package org.enso.pkg

import org.enso.pkg.validation.{InvalidNameError, NameValidation}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListSet

class NameSanitizationSpec extends AnyWordSpec with Matchers {

  "Name Validation" should {

    "sanitize the name of the project" in {
      normalizeName("My_Project") shouldEqual "My_Project"
      normalizeName("My___Project") shouldEqual "My___Project"
      normalizeName("myProject") shouldEqual "MyProject"
      normalizeName("myPro??^ject123") shouldEqual "MyProject123"
      normalizeName("???%$6543lib") shouldEqual "Project_6543lib"
      normalizeName("MyProject™") shouldEqual "MyProject"
      normalizeName("$$$$") shouldEqual "Project"
      normalizeName("$$42$$") shouldEqual "Project_42"
      normalizeName("AoC_1") shouldEqual "AoC_1"
      normalizeName("\uD83D\uDE80") shouldEqual "Project"
      normalizeName("\uD83D\uDE80_\uD83D\uDE0A") shouldEqual "Project"
      normalizeName("\uD83D\uDE80_\uD83D\uDE0A__") shouldEqual "Project"
      normalizeName("_\uD83D\uDE80_\uD83D\uDE0A_") shouldEqual "Project"
    }

    "validate the project name" in {
      validateName("My") shouldEqual Right("My")
      validateName("My_Project") shouldEqual Right("My_Project")
      validateName("") shouldEqual Left(InvalidNameError.Empty)
      validateName("My___Project") shouldEqual Right("My___Project")
      validateName("FooBar") shouldEqual Right("FooBar")
      validateName("myProject") shouldEqual Left(
        InvalidNameError.ShouldStartWithCapitalLetter
      )
      validateName("MyPro??^ject123") shouldEqual Left(
        InvalidNameError.ContainsInvalidCharacters(ListSet('?', '^'))
      )
    }
  }

  private def normalizeName(name: String): String =
    NameValidation.normalizeName(name)

  private def validateName(name: String): Either[InvalidNameError, String] =
    NameValidation.validateName(name)
}

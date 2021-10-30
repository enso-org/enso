package org.enso.pkg

import org.enso.pkg.validation.{InvalidNameError, NameValidation}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ListSet

class NameSanitizationSpec extends AnyWordSpec with Matchers {

  "Name Validation" should {

    "sanitize the name of the project" in {
      normalizeName("My_Project") shouldEqual "My_Project"
      normalizeName("My___Project") shouldEqual "My_Project"
      normalizeName("myProject") shouldEqual "My_Project"
      normalizeName("myPro??^ject123") shouldEqual "My_Project_123"
      normalizeName("???%$6543lib") shouldEqual "Project_6543_Lib"
    }

    "validate the project name" in {
      validateName("My") shouldEqual Right("My")
      validateName("My_Project") shouldEqual Right("My_Project")
      validateName("") shouldEqual Left(InvalidNameError.Empty)
      validateName("My___Project") shouldEqual Left(
        InvalidNameError.ShouldBeUpperSnakeCase("My_Project")
      )
      validateName("FooBar") shouldEqual Left(
        InvalidNameError.ShouldBeUpperSnakeCase("Foo_Bar")
      )
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

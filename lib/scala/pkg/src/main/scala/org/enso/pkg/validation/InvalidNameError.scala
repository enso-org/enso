package org.enso.pkg.validation

sealed trait InvalidNameError
object InvalidNameError {

  case object Empty                        extends InvalidNameError
  case object ShouldStartWithCapitalLetter extends InvalidNameError
  case class ContainsInvalidCharacters(invalidCharacters: Set[Char])
      extends InvalidNameError
  case class ShouldBeUpperSnakeCase(validName: String) extends InvalidNameError
}

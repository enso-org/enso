package org.enso.pkg.validation

/** Base trait for a project name validation errors. */
sealed trait InvalidNameError
object InvalidNameError {

  /** Indicates that project name is empty. */
  case object Empty extends InvalidNameError

  /** Indicates that project name should start with a capital letter. */
  case object ShouldStartWithCapitalLetter extends InvalidNameError

  /** Indicates that projet name contains invalid characters.
    *
    * @param invalidCharacters the list of invalid characters
    */
  case class ContainsInvalidCharacters(invalidCharacters: Set[Char])
      extends InvalidNameError

  /** Indicates that project name should be in upper shane case.
    *
    * @param validName initial project name rewritten in upper snake case
    */
  case class ShouldBeUpperSnakeCase(validName: String) extends InvalidNameError
}

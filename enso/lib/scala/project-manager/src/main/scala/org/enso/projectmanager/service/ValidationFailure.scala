package org.enso.projectmanager.service

/** Base trait for validations failures. */
sealed trait ValidationFailure

object ValidationFailure {

  /** Signals that a user provided empty name. */
  case object EmptyName extends ValidationFailure

  /** Signals that project name should start with a capital letter. */
  case object NameShouldStartWithCapitalLetter extends ValidationFailure

  /** Signals that a project name contains forbidden characters.
    *
    * @param characters a forbidden characters in the provided project name
    */
  case class NameContainsForbiddenCharacter(characters: Set[Char])
      extends ValidationFailure

  /** Signals that project name should be in upper shake case.
    *
    * @param validName initial project name rewritten in upper snake case
    */
  case class NameShouldBeUpperSnakeCase(validName: String)
      extends ValidationFailure
}

package org.enso.projectmanager.service

/** Base trait for validations failures.
  */
sealed trait ValidationFailure

object ValidationFailure {

  /** Signals that a user provided empty name.
    */
  case object EmptyName extends ValidationFailure

  /** Signals that a project name contains forbidden characters.
    *
    * @param characters a forbidden characters in the provided project name
    */
  case class NameContainsForbiddenCharacter(characters: Set[Char])
      extends ValidationFailure

}

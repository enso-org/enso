package org.enso.projectmanager.service.validation

import cats.MonadError
import cats.implicits._
import org.enso.pkg.validation.{InvalidNameError, NameValidation}
import org.enso.projectmanager.service.validation.ModuleNameValidator.ValidationFailure

/** MTL implementation of the module name validator. */
class ModuleNameValidator[F[_, _]](implicit
  M: MonadError[F[ValidationFailure, *], ValidationFailure]
) extends ProjectValidator[F, ValidationFailure] {

  /** Validates a module name.
    *
    * @param name the module name
    * @return either validation failure or success
    */
  override def validate(name: String): F[ValidationFailure, Unit] =
    M.fromEither {
      NameValidation
        .validateName(name)
        .leftMap(toValidationFailure)
        .map(_ => ())
    }

  /** Convert project name error to validation error. */
  private def toValidationFailure(error: InvalidNameError): ValidationFailure =
    error match {
      case InvalidNameError.Empty =>
        ValidationFailure.EmptyName
      case InvalidNameError.ShouldStartWithCapitalLetter =>
        ValidationFailure.NameShouldStartWithCapitalLetter
      case InvalidNameError.ContainsInvalidCharacters(invalidCharacters) =>
        ValidationFailure.NameContainsForbiddenCharacter(invalidCharacters)
      case InvalidNameError.ShouldBeUpperSnakeCase(validName) =>
        ValidationFailure.NameShouldBeUpperSnakeCase(validName)
    }

}

object ModuleNameValidator {

  /** Base trait for validations failures. */
  sealed trait ValidationFailure

  object ValidationFailure {

    /** Signals that a user provided empty name. */
    case object EmptyName extends ValidationFailure

    /** Signals that module name should start with a capital letter. */
    case object NameShouldStartWithCapitalLetter extends ValidationFailure

    /** Signals that a module name contains forbidden characters.
      *
      * @param characters a forbidden characters in the provided module name
      */
    case class NameContainsForbiddenCharacter(characters: Set[Char])
        extends ValidationFailure

    /** Signals that module name should be in upper shake case.
      *
      * @param validName initial module name rewritten in upper snake case
      */
    case class NameShouldBeUpperSnakeCase(validName: String)
        extends ValidationFailure
  }
}

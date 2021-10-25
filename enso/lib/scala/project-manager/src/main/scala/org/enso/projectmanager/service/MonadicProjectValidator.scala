package org.enso.projectmanager.service

import cats.MonadError
import cats.implicits._
import org.enso.pkg.validation.{InvalidNameError, NameValidation}

/** MTL implementation of the project validator. */
class MonadicProjectValidator[F[_, _]](implicit
  M: MonadError[F[ValidationFailure, *], ValidationFailure]
) extends ProjectValidator[F] {

  /** Validates a project name.
    *
    * @param name the project name
    * @return either validation failure or success
    */
  override def validateName(name: String): F[ValidationFailure, Unit] =
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

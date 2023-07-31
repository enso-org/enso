package org.enso.projectmanager.service.validation

import cats.MonadError
import org.enso.projectmanager.service.validation.ProjectNameValidator.ValidationFailure

/** MTL implementation of the project name validator. */
class ProjectNameValidator[F[_, _]](implicit
  M: MonadError[F[ValidationFailure, *], ValidationFailure]
) extends ProjectValidator[F, ValidationFailure] {

  /** Validates a project name.
    *
    * @param name the project name
    * @return either validation failure or success
    */
  override def validate(name: String): F[ValidationFailure, Unit] =
    if (name.trim.isEmpty) M.raiseError(ValidationFailure.EmptyName)
    else M.pure(())
}

object ProjectNameValidator {

  /** Base trait for validations failures. */
  sealed trait ValidationFailure

  object ValidationFailure {

    /** Signals that a user provided empty name. */
    case object EmptyName extends ValidationFailure
  }
}

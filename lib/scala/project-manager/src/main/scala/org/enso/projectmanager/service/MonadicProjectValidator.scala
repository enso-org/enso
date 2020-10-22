package org.enso.projectmanager.service

import cats.MonadError
import cats.implicits._
import org.enso.projectmanager.service.ValidationFailure.{
  EmptyName,
  NameContainsForbiddenCharacter
}

/** MTL implementation of the project validator.
  */
class MonadicProjectValidator[F[_, _]](implicit
  M: MonadError[F[ValidationFailure, *], ValidationFailure]
) extends ProjectValidator[F] {

  import M._

  private val validCharSpec: Char => Boolean = { char =>
    char.isLetterOrDigit || char == '_' || char == '-'
  }

  /** Validates a project name.
    *
    * @param name the project name
    * @return either validation failure or success
    */
  override def validateName(name: String): F[ValidationFailure, Unit] =
    checkIfNonEmptyName(name) *> checkCharacters(name)

  private def checkIfNonEmptyName(name: String): F[ValidationFailure, Unit] =
    if (name.trim.isEmpty) {
      raiseError(EmptyName)
    } else {
      unit
    }

  private def checkCharacters(name: String): F[ValidationFailure, Unit] = {
    val forbiddenChars = name.toCharArray.filterNot(validCharSpec).toSet
    if (forbiddenChars.isEmpty) unit
    else raiseError(NameContainsForbiddenCharacter(forbiddenChars))
  }

}

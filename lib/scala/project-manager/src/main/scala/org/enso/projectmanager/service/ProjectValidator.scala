package org.enso.projectmanager.service

/** Abstraction for project validator.
  *
  * @tparam F target bifunctor
  */
trait ProjectValidator[F[_, _]] {

  /** Validates a project name.
    *
    * @param name the project name
    * @return either validation failure or success
    */
  def validateName(name: String): F[ValidationFailure, Unit]

}

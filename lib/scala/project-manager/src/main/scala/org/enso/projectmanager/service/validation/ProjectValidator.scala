package org.enso.projectmanager.service.validation

/** Abstraction for project validator.
  *
  * @tparam F target bifunctor
  * @tparam E validation error
  */
trait ProjectValidator[F[_, _], E] {

  /** Validates a project.
    *
    * @param name the name to validate
    * @return either validation failure or success
    */
  def validate(name: String): F[E, Unit]
}

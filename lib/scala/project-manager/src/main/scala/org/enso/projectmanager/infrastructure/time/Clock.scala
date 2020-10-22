package org.enso.projectmanager.infrastructure.time

import java.time.OffsetDateTime

/** A clock abstraction providing pure access to the current date and time
  * using a time-zone.
  *
  * @tparam F monadic context
  */
trait Clock[F[+_, +_]] {

  /** Obtains the current date-time from the system clock in the default time-zone.
    *
    * @return a date time
    */
  def now(): F[Nothing, OffsetDateTime]

  /** Obtains the current date-time from the system clock in the UTC time-zone.
    *
    * @return a date time
    */
  def nowInUtc(): F[Nothing, OffsetDateTime]

}

package org.enso.projectmanager.infrastructure.time
import java.time.{OffsetDateTime, ZoneOffset}

import org.enso.projectmanager.control.effect.Sync

/**
  * A system clock.
  */
class RealClock[F[+_, +_]: Sync] extends Clock[F] {

  /**
    * Obtains the current date-time from the system clock in the default time-zone.
    *
    * @return a date time
    */
  override def now(): F[Nothing, OffsetDateTime] =
    Sync[F].effect(OffsetDateTime.now())

  /**
    * Obtains the current date-time from the system clock in the UTC time-zone.
    *
    * @return a date time
    */
  override def nowInUtc(): F[Nothing, OffsetDateTime] =
    Sync[F].effect(OffsetDateTime.now(ZoneOffset.UTC))

}

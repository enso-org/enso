package org.enso.projectmanager.infrastructure.random
import java.util.UUID

import org.enso.projectmanager.control.effect.Sync

/**
  * A system pseudo-random numbers generator.
  */
class SystemGenerator[F[+_, +_]: Sync] extends Generator[F] {

  /**
    * Returns random UUID in version 4.
    *
    * @return a UUID
    */
  override def randomUUID(): F[Nothing, UUID] =
    Sync[F].effect(UUID.randomUUID())
}

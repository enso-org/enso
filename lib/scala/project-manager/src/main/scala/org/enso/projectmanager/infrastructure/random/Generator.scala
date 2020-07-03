package org.enso.projectmanager.infrastructure.random

import java.util.UUID

/**
  * A pure functional abstraction for random generation.
  *
  * @tparam F a monadic context
  */
trait Generator[F[+_, +_]] {

  /**
    * Returns random UUID in version 4.
    *
    * @return a UUID
    */
  def randomUUID(): F[Nothing, UUID]

}

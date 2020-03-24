package org.enso.projectmanager.test

import java.time.{OffsetDateTime, ZoneOffset}

import org.enso.projectmanager.infrastructure.time.Clock
import zio.{IO, ZIO}

class StoppedClock[R](now: OffsetDateTime) extends Clock[ZIO[R, +*, +*]] {

  override def now(): IO[Nothing, OffsetDateTime] = IO.succeed(now)

  override def nowInUtc(): IO[Nothing, OffsetDateTime] =
    IO.succeed(now.withOffsetSameInstant(ZoneOffset.UTC))

}

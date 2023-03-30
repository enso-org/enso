package org.enso.projectmanager.test

import java.time.{OffsetDateTime, ZoneOffset}

import org.enso.projectmanager.infrastructure.time.Clock
import zio.{IO, ZIO}

class ProgrammableClock[R](initialNow: OffsetDateTime)
    extends Clock[ZIO[R, +*, +*]] {

  @volatile
  var currentTime = initialNow.withOffsetSameInstant(ZoneOffset.UTC)

  override def now(): IO[Nothing, OffsetDateTime] = ZIO.succeed(currentTime)

  override def nowInUtc(): IO[Nothing, OffsetDateTime] =
    ZIO.succeed(currentTime)

  def moveTimeForward(seconds: Long = 1): Unit =
    currentTime = currentTime.plusSeconds(seconds)

}

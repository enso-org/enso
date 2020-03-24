package org.enso.projectmanager.test

import java.util.UUID

import org.enso.projectmanager.infrastructure.random.Generator
import zio.{IO, ZIO}

class ConstGenerator[R](testUUID: UUID) extends Generator[ZIO[R, +*, +*]] {
  override def randomUUID(): IO[Nothing, UUID] = IO.succeed(testUUID)
}

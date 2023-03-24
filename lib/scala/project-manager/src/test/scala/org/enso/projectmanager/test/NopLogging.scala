package org.enso.projectmanager.test

import org.enso.projectmanager.infrastructure.log.Logging
import zio.{IO, ZIO}

class NopLogging[R] extends Logging[ZIO[R, +*, +*]] {

  override def debug(msg: String): IO[Nothing, Unit] = ZIO.unit

  override def debug(msg: String, args: AnyRef*): IO[Nothing, Unit] = ZIO.unit

  override def info(msg: String): IO[Nothing, Unit] = ZIO.unit

  override def info(msg: String, args: AnyRef*): IO[Nothing, Unit] = ZIO.unit

  override def error(msg: String): IO[Nothing, Unit] = ZIO.unit

  override def error(msg: String, args: AnyRef*): IO[Nothing, Unit] = ZIO.unit
}

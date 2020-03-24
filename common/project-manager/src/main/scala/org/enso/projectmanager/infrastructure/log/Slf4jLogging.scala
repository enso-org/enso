package org.enso.projectmanager.infrastructure.log
import com.typesafe.scalalogging.LazyLogging
import org.enso.projectmanager.control.effect.Sync

/**
  * Slf4j logging interpreter.
  */
class Slf4jLogging[F[+_, +_]: Sync] extends Logging[F] with LazyLogging {

  override def debug(msg: String): F[Nothing, Unit] =
    Sync[F].effect(logger.debug(msg))

  override def info(msg: String): F[Nothing, Unit] =
    Sync[F].effect(logger.info(msg))

  override def error(msg: String): F[Nothing, Unit] =
    Sync[F].effect(logger.error(msg))

}

package org.enso.projectmanager.control.effect

import zio._

import scala.concurrent.{Future, Promise}

/** ZIO executor.
  *
  * @param runtime a ZIO runtime
  */
class ZioEnvExec(runtime: Runtime[ZEnv]) extends Exec[ZIO[ZEnv, *, *]] {

  override def exec[E, A](op: ZIO[ZEnv, E, A]): Future[Either[E, A]] = {
    val promise = Promise[Either[E, A]]()
    runtime.unsafeRunAsync(op) {
      _.fold(
        { cause =>
          cause.failureOption match {
            case Some(e) =>
              promise.success(Left(e))
            case None =>
              val error = cause.defects.headOption
                .getOrElse(new RuntimeException("ZIO failed"))
              promise.failure(error)
          }
        },
        r => promise.success(Right(r))
      )
    }
    promise.future
  }

}

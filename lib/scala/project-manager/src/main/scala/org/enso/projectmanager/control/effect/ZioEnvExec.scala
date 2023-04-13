package org.enso.projectmanager.control.effect

import zio._

import scala.concurrent.{Future, Promise}

/** ZIO executor.
  *
  * @param runtime a ZIO runtime
  */
class ZioEnvExec(runtime: Runtime[ZAny]) extends Exec[ZIO[ZAny, *, *]] {

  /** @inheritdoc */
  override def exec[E, A](op: ZIO[ZAny, E, A]): Future[Either[E, A]] =
    zio.Unsafe.unsafe { implicit unsafe =>
      val promise = Promise[Either[E, A]]()
      runtime.unsafe.fork(
        op.foldCause(
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
      )
      promise.future
    }
}

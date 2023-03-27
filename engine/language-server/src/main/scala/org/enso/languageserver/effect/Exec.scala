package org.enso.languageserver.effect

import org.enso.languageserver.effect
import zio._

import java.util.concurrent.{ExecutionException, TimeoutException}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

/** Abstract entity that executes effects `F`.
  */
trait Exec[-F[_, _]] {

  /** Execute Zio effect.
    *
    * @param op effect to execute
    * @return a future containing either a failure or a result
    */
  def exec[E, A](op: F[E, A])(implicit trace: Trace): Future[Either[E, A]]

  /** Execute Zio effect with timeout.
    *
    * @param timeout execution timeout
    * @param op effect to execute
    * @return a future
    */
  def execTimed[E, A](
    timeout: FiniteDuration,
    op: F[E, A]
  )(implicit trace: Trace): Future[Either[E, A]]

  /** Execute long running task in background.
    *
    * @param op effect to execute
    */
  def exec_[E <: Throwable, A](op: F[E, A])(implicit trace: Trace): Unit
}

/** Executor of Zio effects.
  *
  * @param runtime zio runtime
  */
case class ZioExec(runtime: effect.Runtime) extends Exec[ZioExec.IO] {

  /** Execute Zio effect.
    *
    * @param op effect to execute
    * @param trace object prevents the library from messing up the user's execution trace
    * @return a future containing either a failure or a result
    */
  override def exec[E, A](
    op: ZIO[ZAny, E, A]
  )(implicit trace: Trace): Future[Either[E, A]] =
    zio.Unsafe.unsafe { implicit unsafe =>
      val promise = Promise[Either[E, A]]()
      runtime.instance.unsafe.fork(
        op.foldCause(
          ZioExec.completeFailure(promise, _),
          ZioExec.completeSuccess(promise, _)
        )
      )
      promise.future
    }

  /** Execute Zio effect with timeout.
    *
    * @param timeout execution timeout
    * @param op effect to execute
    * @param trace object prevents the library from messing up the user's execution trace
    * @return a future. On timeout future is failed with `TimeoutException`.
    * Otherwise future contains either a failure or a result.
    */
  override def execTimed[E, A](
    timeout: FiniteDuration,
    op: ZIO[ZAny, E, A]
  )(implicit trace: Trace): Future[Either[E, A]] =
    zio.Unsafe.unsafe { implicit unsafe =>
      val promise = Promise[Either[E, A]]()
      runtime.instance.unsafe.fork(
        op.disconnect
          .timeout(zio.Duration.fromScala(timeout))
          .foldCause[Unit](
            ZioExec.completeFailure(promise, _),
            _.fold(promise.failure(ZioExec.timeoutFailure))(a =>
              promise.success(Right(a))
            )
          )
      )
      promise.future
    }

  /** Execute long running task in background.
    *
    * @param op effect to execute
    * @param trace object prevents the library from messing up the user's execution trace
    */
  override def exec_[E <: Throwable, A](
    op: ZIO[ZAny, E, A]
  )(implicit trace: Trace): Unit =
    zio.Unsafe.unsafe { implicit unsafe =>
      runtime.instance.unsafe.fork(ZIO.blocking(op))
    }
}

object ZioExec {

  type IO[+E, +A] = ZIO[ZAny, E, A]

  object ZioExecutionException extends ExecutionException

  private def completeSuccess[E, A](
    promise: Promise[Either[E, A]],
    result: A
  ): Unit =
    promise.success(Right(result))

  private def completeFailure[E, A](
    promise: Promise[Either[E, A]],
    cause: Cause[E]
  ): Unit =
    cause.failureOption match {
      case Some(e) =>
        promise.success(Left(e))
      case None =>
        val error = cause.defects.headOption.getOrElse(executionFailure)
        promise.failure(error)
    }

  private val executionFailure: Throwable =
    new ExecutionException("ZIO execution failed", ZioExecutionException)

  private val timeoutFailure: Throwable =
    new TimeoutException()
}

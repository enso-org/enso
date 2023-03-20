package org.enso.languageserver.effect

import org.enso.languageserver.effect
import zio._
import zio.blocking.blocking

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
  def exec[E, A](op: F[E, A]): Future[Either[E, A]]

  /** Execute Zio effect with timeout.
    *
    * @param timeout execution timeout
    * @param op effect to execute
    * @return a future
    */
  def execTimed[E, A](
    timeout: FiniteDuration,
    op: F[E, A]
  ): Future[Either[E, A]]

  /** Execute long running task in background.
    *
    * @param op effect to execute
    */
  def exec_[E, A](op: F[E, A]): Unit
}

/** Executor of Zio effects.
  *
  * @param runtime zio runtime
  */
case class ZioExec(runtime: effect.Runtime) extends Exec[ZioExec.IO] {

  /** Execute Zio effect.
    *
    * @param op effect to execute
    * @return a future containing either a failure or a result
    */
  override def exec[E, A](op: ZIO[ZEnv, E, A]): Future[Either[E, A]] = {
    val promise = Promise[Either[E, A]]()
    runtime.instance.unsafeRunAsync(op) {
      _.fold(
        ZioExec.completeFailure(promise, _),
        ZioExec.completeSuccess(promise, _)
      )
    }
    promise.future
  }

  /** Execute Zio effect with timeout.
    *
    * @param timeout execution timeout
    * @param op effect to execute
    * @return a future. On timeout future is failed with `TimeoutException`.
    * Otherwise future contains either a failure or a result.
    */
  override def execTimed[E, A](
    timeout: FiniteDuration,
    op: ZIO[ZEnv, E, A]
  ): Future[Either[E, A]] = {
    val promise = Promise[Either[E, A]]()
    runtime.instance.unsafeRunAsync(
      op.disconnect.timeout(zio.duration.Duration.fromScala(timeout))
    ) {
      _.fold(
        ZioExec.completeFailure(promise, _),
        _.fold(promise.failure(ZioExec.timeoutFailure))(a =>
          promise.success(Right(a))
        )
      )
    }
    promise.future
  }

  /** Execute long running task in background.
    *
    * @param op effect to execute
    */
  override def exec_[E, A](op: ZIO[ZEnv, E, A]): Unit =
    runtime.instance.unsafeRunAsync_(blocking(op))
}

object ZioExec {

  type IO[+E, +A] = ZIO[ZEnv, E, A]

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

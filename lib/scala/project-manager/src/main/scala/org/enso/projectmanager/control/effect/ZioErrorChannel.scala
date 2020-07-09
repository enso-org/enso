package org.enso.projectmanager.control.effect

import shapeless.=:!=
import zio.{CanFail, ZIO}

import scala.annotation.unused

/**
  * Instance of [[ErrorChannel]] class for ZIO.
  */
class ZioErrorChannel[R] extends ErrorChannel[ZIO[R, +*, +*]] {

  implicit private def canFailEv[E](
    implicit @unused ev: E =:!= Nothing
  ): CanFail[E] =
    CanFail

  /** @inheritdoc **/
  override def recover[E, A, B >: A](fa: ZIO[R, E, A])(
    recovery: PartialFunction[E, B]
  ): ZIO[R, E, B] =
    recoverWith[E, A, B, E](fa)(recovery.andThen(ZIO.succeed(_)))

  /** @inheritdoc **/
  override def recoverWith[E, A, B >: A, E1 >: E](fa: ZIO[R, E, A])(
    recovery: PartialFunction[E, ZIO[R, E1, B]]
  ): ZIO[R, E1, B] =
    fa.foldM(
      failure = { error =>
        if (recovery.isDefinedAt(error)) recovery(error)
        else ZIO.fail(error)
      },
      success = ZIO.succeed(_)
    )

  /** @inheritdoc **/
  override def fallbackTo[E, A, B >: A, E1](fa: ZIO[R, E, A])(
    fallback: E => ZIO[R, E1, B]
  ): ZIO[R, E1, B] =
    fa.foldM(
      failure = { error => fallback(error) },
      success = ZIO.succeed(_)
    )

  /** @inheritdoc **/
  override def liftEither[E, A](either: Either[E, A]): ZIO[R, E, A] =
    ZIO.fromEither(either)

  /** @inheritdoc **/
  override def mapError[E, A, E1](
    fa: ZIO[R, E, A]
  )(f: E => E1)(implicit ev: E =:!= Nothing): ZIO[R, E1, A] =
    fa.mapError(f)

  /** @inheritdoc **/
  override def fail[E](error: => E): ZIO[R, E, Nothing] = ZIO.fail(error)

  /** @inheritdoc **/
  override def onError[E, A](
    fa: ZIO[R, E, A]
  )(cleanUp: PartialFunction[E, ZIO[R, Nothing, Unit]]): ZIO[R, E, A] =
    fa.onError { cause =>
      if (cause.failed) {
        val failure = cause.failureOption.get
        if (cleanUp.isDefinedAt(failure)) cleanUp(failure)
        else ZIO.unit
      } else {
        ZIO.unit
      }
    }

  /** @inheritdoc **/
  override def onDie[E, A](
    fa: ZIO[R, E, A]
  )(cleanUp: PartialFunction[Throwable, ZIO[R, Nothing, Unit]]): ZIO[R, E, A] =
    fa.onError { cause =>
      if (cause.died) {
        val throwable = cause.dieOption.get
        if (cleanUp.isDefinedAt(throwable)) cleanUp(throwable)
        else ZIO.unit
      } else {
        ZIO.unit
      }
    }

}

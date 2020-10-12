package org.enso.projectmanager.control.core

import zio.ZIO

/**
  * An instance of [[Applicative]] class for ZIO.
  */
private[core] class ZioApplicative[R] extends Applicative[ZIO[R, +*, +*]] {

  /** @inheritdoc */
  override def pure[E, A](a: A): ZIO[R, E, A] =
    ZIO.succeed(a)

  /** @inheritdoc */
  override def ap[E, E1 >: E, A, B](
    ff: ZIO[R, E, A => B]
  )(fa: ZIO[R, E1, A]): ZIO[R, E1, B] =
    ff.flatMap(f => fa.map(f))

  /** @inheritdoc */
  override def map[E, A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] =
    fa.map(f)

  /** @inheritdoc */
  override def map2[E, A, B, C](ma: ZIO[R, E, A], mb: ZIO[R, E, B])(
    f: (A, B) => C
  ): ZIO[R, E, C] =
    ma.flatMap(a => mb.map(b => f(a, b)))
}

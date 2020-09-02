package org.enso.projectmanager.control.core

import zio.ZIO

/**
  * Instance of [[CovariantFlatMap]] class for ZIO.
  */
private[core] class ZioCovariantFlatMap[R]
    extends CovariantFlatMap[ZIO[R, +*, +*]] {

  /** @inheritdoc */
  override def pure[A](value: A): ZIO[R, Nothing, A] = ZIO.succeed(value)

  /** @inheritdoc */
  override def flatMap[E1, E2 >: E1, A, B](
    fa: ZIO[R, E1, A]
  )(f: A => ZIO[R, E2, B]): ZIO[R, E2, B] =
    fa.flatMap(f)

  /** @inheritdoc */
  override def traverse[E, A, B](
    s: Iterable[A]
  )(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    ZIO.foreach(s)(f)

}

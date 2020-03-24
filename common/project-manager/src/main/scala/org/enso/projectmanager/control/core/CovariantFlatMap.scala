package org.enso.projectmanager.control.core

import zio._

/**
  * A class for covariant effects containing error channel used to chain
  * computations.
  *
  * @tparam F a effectful context
  */
trait CovariantFlatMap[F[+_, +_]] {

  /**
    * Lifts any value into a context F[Nothing, A].
    *
    * @param value a value to lift
    * @return
    */
  def pure[A](value: A): F[Nothing, A]

  /**
    * Chains `fa` with `f` arrow.
    *
    * @param fa an effectful computation
    * @param f dependent effectful function
    * @return a composition of dependent effectful functions
    */
  def flatMap[E1, E2 >: E1, A, B](fa: F[E1, A])(f: A => F[E2, B]): F[E2, B]

}

object CovariantFlatMap {

  def apply[F[+_, +_]](
    implicit covariantFlatMap: CovariantFlatMap[F]
  ): CovariantFlatMap[F] =
    covariantFlatMap

  implicit def zioFlatMap[R]: CovariantFlatMap[ZIO[R, +*, +*]] =
    zioFlatMapInstance
      .asInstanceOf[CovariantFlatMap[ZIO[R, +*, +*]]]

  final private[this] val zioFlatMapInstance
    : CovariantFlatMap[ZIO[Any, +*, +*]] =
    new ZioCovariantFlatMap[Any]

}

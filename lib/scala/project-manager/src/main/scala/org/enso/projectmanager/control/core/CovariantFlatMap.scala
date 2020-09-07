package org.enso.projectmanager.control.core

import zio._

import scala.util.{Either, Left, Right}

/**
  * A class for covariant effects containing error channel used to chain
  * computations.
  *
  * @tparam F an effectful context
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

  /**
    * `if` lifted into monad.
    */
  def ifM[E, B](
    fa: F[E, Boolean]
  )(ifTrue: => F[E, B], ifFalse: => F[E, B]): F[E, B] =
    flatMap(fa)(if (_) ifTrue else ifFalse)

  /**
    * Keeps calling `f` until a `scala.util.Right[B]` is returned.
    */
  def tailRecM[E, A, B](a: A)(f: A => F[E, Either[A, B]]): F[E, B] =
    flatMap(f(a)) {
      case Left(a)  => tailRecM(a)(f)
      case Right(b) => pure(b)
    }

}

object CovariantFlatMap {

  def apply[F[+_, +_]](implicit
    covariantFlatMap: CovariantFlatMap[F]
  ): CovariantFlatMap[F] =
    covariantFlatMap

  implicit def zioFlatMap[R]: CovariantFlatMap[ZIO[R, +*, +*]] =
    zioFlatMapInstance
      .asInstanceOf[CovariantFlatMap[ZIO[R, +*, +*]]]

  final private[this] val zioFlatMapInstance
    : CovariantFlatMap[ZIO[Any, +*, +*]] =
    new ZioCovariantFlatMap[Any]

}

package org.enso.projectmanager.control.core

object syntax {

  /**
    * Implicit conversion to [[CovariantFlatMapOps]]
    *
    * @param fa an effect
    * @return
    */
  implicit def toCovariantFlatMapOps[F[+_, +_]: CovariantFlatMap, E, A](
    fa: F[E, A]
  ): CovariantFlatMapOps[F, E, A] = new CovariantFlatMapOps[F, E, A](fa)

  /**
    * Implicit conversion to [[ApplicativeOps]].
    */
  implicit def toApplicativeOps[F[+_, +_]: Applicative, E, A](
    fa: F[E, A]
  ): ApplicativeOps[F, E, A] =
    new ApplicativeOps[F, E, A](fa)

  /**
    * Implicit conversion to [[ApplicativeFunctionOps]].
    */
  implicit def toApplicativeFunctionOps[F[+_, +_]: Applicative, E, A, B](
    ff: F[E, A => B]
  ): ApplicativeFunctionOps[F, E, A, B] =
    new ApplicativeFunctionOps[F, E, A, B](ff)
}

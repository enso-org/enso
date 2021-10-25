package org.enso.projectmanager.control.core

class ApplicativeFunctionOps[F[+_, +_]: Applicative, E, A, B](
  ff: F[E, A => B]
) {

  /** Alias for [[Applicative.ap]]. */
  def <*>[E1 <: E](fa: F[E1, A]): F[E, B] =
    Applicative[F].ap(ff)(fa)
}

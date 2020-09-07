package org.enso.projectmanager.control.core

class ApplicativeOps[F[+_, +_]: Applicative, E, A](fa: F[E, A]) {

  /** Compose two actions, discarding any value produced by the first. */
  def *>[E1 >: E, B](fb: F[E, B]): F[E1, B] =
    Applicative[F].ap(Applicative[F].map(fa)(_ => (b: B) => b))(fb)

  /** Compose two actions, discarding any value produced by the second. */
  def <*[E1 >: E, B](fb: F[E1, B]): F[E1, A] =
    Applicative[F].ap(Applicative[F].map(fb)(_ => (a: A) => a))(fa)
}

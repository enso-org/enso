package org.enso.projectmanager.control.core

import zio.ZIO

/**
  * An applicative functor.
  */
trait Applicative[F[+_, +_]] {

  /**
    * Lifts value into the applicative functor.
    */
  def pure[E, A](a: A): F[E, A]

  /**
    * Applies the function in the applicative context to the given value.
    */
  def ap[E, E1 >: E, A, B](ff: F[E, A => B])(fa: F[E1, A]): F[E1, B]

  /**
    * Applies function f to the given value.
    */
  def map[E, A, B](fa: F[E, A])(f: A => B): F[E, B]

  /**
    * Applies the binary function to the effectful falues ma and mb.
    */
  def map2[E, A, B, C](ma: F[E, A], mb: F[E, B])(f: (A, B) => C): F[E, C]
}

object Applicative {

  def apply[F[+_, +_]](implicit applicative: Applicative[F]): Applicative[F] =
    applicative

  implicit def zioApplicative[R]: Applicative[ZIO[R, +*, +*]] =
    zioApplicativeInstance.asInstanceOf[Applicative[ZIO[R, +*, +*]]]

  final private[this] val zioApplicativeInstance
    : Applicative[ZIO[Any, +*, +*]] =
    new ZioApplicative[Any]

}

package org.enso.projectmanager.control.core

class CovariantFlatMapOps[F[+_, +_]: CovariantFlatMap, E, A](fa: F[E, A]) {

  /** @inheritdoc */
  def flatMap[E1 >: E, B](f: A => F[E1, B]): F[E1, B] =
    CovariantFlatMap[F].flatMap[E, E1, A, B](fa)(f)

  /** @inheritdoc */
  def map[B](f: A => B): F[E, B] = flatMap(a => CovariantFlatMap[F].pure(f(a)))

  /** @inheritdoc */
  def >>=[E1 >: E, B](f: A => F[E1, B]): F[E1, B] = flatMap(f)

  /** @inheritdoc */
  def *>[E1 >: E, B](f: F[E1, B]): F[E1, B] = flatMap(_ => f)

  /** @inheritdoc */
  def >>[E1 >: E, B](f: F[E1, B]): F[E1, B] = flatMap(_ => f)

}

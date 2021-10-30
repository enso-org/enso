package org.enso.projectmanager.control.effect

import scala.concurrent.Future
import scala.util.Either

/** A class for asynchronous effects that do not block threads.
  *
  * @tparam F an effectful context
  */
trait Async[F[+_, +_]] {

  /** Imports an asynchronous side-effect into a pure `F` value.
    *
    * @param register is a function that should be called with a
    *                 callback for signaling the result once it is ready
    * @tparam E an error type
    * @tparam A a result type
    * @return pure `F` value
    */
  def async[E, A](register: (Either[E, A] => Unit) => Unit): F[E, A]

  /** Converts side-effecting future into a pure `F` value.
    *
    * @param thunk a thunk that starts asynchronous computations
    * @tparam A a returned type
    * @return pure `F` value
    */
  def fromFuture[A](thunk: () => Future[A]): F[Throwable, A]

}

object Async {

  def apply[F[+_, +_]](implicit async: Async[F]): Async[F] = async

  implicit def zioAsync[R]: ZioAsync[R] = new ZioAsync[R]

}

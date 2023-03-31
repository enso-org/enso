package org.enso.projectmanager.control.effect

import java.io.IOException

import zio.{ZAny, ZIO}

import scala.concurrent.duration.FiniteDuration

/** A class for synchronous effects that block threads.
  *
  * @tparam F an effectful context
  */
trait Sync[F[+_, +_]] {

  /** Lifts a total synchronous effect into a pure `F` context.
    *
    * @param effect an effect
    * @return
    */
  def effect[A](effect: => A): F[Nothing, A]

  /** Lifts a blocking operation into a pure `F` context.
    *
    * @param effect an effect
    * @return
    */
  def blockingOp[A](effect: => A): F[Throwable, A]

  /** Lifts a blocking IO operation into a pure `F` context.
    *
    * @param effect an effect
    * @return
    */
  def blockingIO[A](effect: => A): F[IOException, A]

  /** Returns an effect that will timeout `fa` effect.
    *
    * @param fa an effectful computation
    * @param e a timeout error
    * @param timeout a timeout
    * @return
    */
  def timeoutFail[E, E1 >: E, A](fa: F[E, A])(e: E1)(
    timeout: FiniteDuration
  ): F[E1, A]

}

object Sync {

  def apply[F[+_, +_]](implicit sync: Sync[F]): Sync[F] = sync

  implicit val zioSync: Sync[ZIO[ZAny, +*, +*]] = ZioSync

}

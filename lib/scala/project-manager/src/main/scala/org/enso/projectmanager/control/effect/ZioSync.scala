package org.enso.projectmanager.control.effect

import java.io.IOException

import zio._
import zio.blocking
import zio.duration.Duration

import scala.concurrent.duration.FiniteDuration

/**
  * Instance of [[Sync]] class for ZIO.
  */
object ZioSync extends Sync[ZIO[ZEnv, +*, +*]] {

  /** @inheritdoc **/
  override def effect[A](effect: => A): ZIO[ZEnv, Nothing, A] =
    ZIO.effectTotal(effect)

  /** @inheritdoc **/
  override def blockingOp[A](effect: => A): ZIO[ZEnv, Throwable, A] =
    blocking.effectBlocking(effect)

  /** @inheritdoc **/
  override def blockingIO[A](effect: => A): ZIO[ZEnv, IOException, A] =
    blocking.effectBlockingIO(effect)

  /** @inheritdoc **/
  override def timeoutFail[E, E1 >: E, A](fa: ZIO[ZEnv, E, A])(e: E1)(
    timeout: FiniteDuration
  ): ZIO[zio.ZEnv, E1, A] = fa.timeoutFail(e)(Duration.fromScala(timeout))

}

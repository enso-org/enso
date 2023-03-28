package org.enso.projectmanager.control.effect

import java.io.IOException

import zio._
import zio.Duration

import scala.concurrent.duration.FiniteDuration
import zio.ZIO

/** Instance of [[Sync]] class for ZIO.
  */
object ZioSync extends Sync[ZIO[ZAny, +*, +*]] {

  /** @inheritdoc */
  override def effect[A](effect: => A): ZIO[ZAny, Nothing, A] =
    ZIO.succeed(effect)

  /** @inheritdoc */
  override def blockingOp[A](effect: => A): ZIO[ZAny, Throwable, A] =
    ZIO.attemptBlocking(effect)

  /** @inheritdoc */
  override def blockingIO[A](effect: => A): ZIO[ZAny, IOException, A] =
    ZIO.attemptBlockingIO(effect)

  /** @inheritdoc */
  override def timeoutFail[E, E1 >: E, A](fa: ZIO[ZAny, E, A])(e: E1)(
    timeout: FiniteDuration
  ): ZIO[ZAny, E1, A] = fa.timeoutFail(e)(Duration.fromScala(timeout))

}

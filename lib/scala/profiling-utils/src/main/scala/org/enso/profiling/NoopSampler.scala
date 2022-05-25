package org.enso.profiling

import java.util.concurrent.Executor

import scala.concurrent.duration.FiniteDuration

/** Sampler that does nothing. */
final class NoopSampler extends MethodsSampler {

  /** @inheritdoc */
  override def start(): Unit = ()

  /** @inheritdoc */
  override def stop(): Unit = ()

  /** @inheritdoc */
  override def stop(delay: FiniteDuration)(implicit ec: Executor): Unit = ()
}
object NoopSampler {

  /** Create an instance of [[NoopSampler]]. */
  def apply(): NoopSampler =
    new NoopSampler
}

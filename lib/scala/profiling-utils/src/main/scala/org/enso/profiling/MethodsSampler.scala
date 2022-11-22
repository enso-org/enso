package org.enso.profiling

import java.util.concurrent.Executor

import scala.concurrent.duration.FiniteDuration

/** Sampler gathers the application performance statistics. */
trait MethodsSampler {

  /** Start gathering the application statistics. */
  def start(): Unit

  /** Stop gathering the application statistics and write it to the output. */
  def stop(): Unit

  /** Stop gathering the application statistics after the provided delay and
    * write it to the output.
    *
    * @param delay the duration to wait before stopping
    * @param ec the execution context
    */
  def stop(delay: FiniteDuration)(implicit ec: Executor): Unit
}

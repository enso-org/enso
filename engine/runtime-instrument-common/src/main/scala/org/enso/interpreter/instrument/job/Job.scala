package org.enso.interpreter.instrument.job

import com.oracle.truffle.api.TruffleLogger

import java.util.UUID
import org.enso.interpreter.instrument.execution.RuntimeContext

import java.util.logging.Level

/** A fine-grained request for a runtime server service. Uses [[RuntimeContext]]
  * to perform a request.
  *
  * @param contextIds affected executions contexts' ids
  * @param isCancellable says if the job is cancellable
  * @param mayInterruptIfRunning determines if the job may be interrupted when
  *                              running
  */
abstract class Job[+A](
  val contextIds: List[UUID],
  val isCancellable: Boolean,
  val mayInterruptIfRunning: Boolean
) {

  /** Executes a job.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def run(implicit ctx: RuntimeContext): A

  protected def logLockRelease(
    logger: TruffleLogger,
    name: String,
    startTime: Long,
    release: => Unit
  ): Unit = {
    Job.logLockRelease(logger, name, startTime, release)
  }

}

object Job {
  protected[job] def logLockRelease(
    logger: TruffleLogger,
    name: String,
    startTime: Long,
    release: => Unit
  ): Unit = {
    if (startTime != 0) {
      release
      logger.log(
        Level.FINEST,
        "Kept {0} lock [{1}] for {2} milliseconds",
        Array(
          name,
          getClass.getSimpleName,
          System.currentTimeMillis - startTime
        )
      )
    }
  }
}

/** The job queue can contain only one job of this type decided by the
  * `equalsTo` method. When a job of this type is added to the job queue,
  * previous duplicate jobs are cancelled.
  */
trait UniqueJob[A] { self: Job[A] =>

  /** Decide if this job is the same as the other job.
    *
    * @param that the other job to compare with
    * @return `true` if `this` job is considered the same as `that` job
    */
  def equalsTo(that: UniqueJob[_]): Boolean
}

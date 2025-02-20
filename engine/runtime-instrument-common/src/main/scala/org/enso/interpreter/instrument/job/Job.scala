package org.enso.interpreter.instrument.job

import java.util.UUID

import org.enso.interpreter.instrument.execution.RuntimeContext

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
  val mayInterruptIfRunning: Boolean,
  val highPriority: Boolean
) {

  @volatile private var _hasStarted = false

  def this(
    contextIds: List[UUID],
    isCancellable: Boolean,
    mayInterruptIfRunning: Boolean
  ) = {
    this(contextIds, isCancellable, mayInterruptIfRunning, false)
  }

  /** Executes a job. Will mark the job as "started".
    *
    * @param ctx contains suppliers of services to perform a request
    */
  final def run(implicit ctx: RuntimeContext): A = {
    _hasStarted = true
    runImpl(ctx)
  }

  /** Executes a job.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def runImpl(implicit ctx: RuntimeContext): A

  /** Returns the name of the thread which executes the job, if any.
    * @return a name of the thread or null, if information is unsupported
    */
  def threadNameExecutingJob(): String = null

  /** Indicates whether the job has started executing. */
  def hasStarted(): Boolean = {
    _hasStarted
  }

  private[instrument] def setJobId(id: UUID): Unit = ()

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

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
  val mayInterruptIfRunning: Boolean
) {

  /** Executes a job.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  def run(implicit ctx: RuntimeContext): A

  override def toString: String = this.getClass.getSimpleName
}

/** The job queue can contain only one job of this type with the same `key`.
  * When a job of this type is added to the job queue, previous duplicate jobs
  * are cancelled.
  *
  * @param key a unique job key
  * @param contextIds affected executions contests' ids
  * @param mayInterruptIfRunning determines if the job may be interruptd when
  *                              running
  */
abstract class UniqueJob[+A](
  val key: UUID,
  contextIds: List[UUID],
  mayInterruptIfRunning: Boolean
) extends Job[A](contextIds, isCancellable = false, mayInterruptIfRunning)

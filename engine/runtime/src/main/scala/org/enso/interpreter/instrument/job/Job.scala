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

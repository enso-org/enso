package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.job.Job

import java.util.UUID

/** Controls running jobs.
  */
trait JobControlPlane {

  /** Aborts all interruptible jobs. */
  def abortAllJobs(): Unit

  /** Abort all jobs except the ignored jobs.
    *
    * @param ignoredJobs the list of jobs to keep in the execution queue
    */
  def abortAllExcept(ignoredJobs: Class[_ <: Job[_]]*): Unit

  /** Aborts all jobs that relates to the specified execution context.
    *
    * @param contextId an identifier of a context
    */
  def abortJobs(contextId: UUID): Unit

  /** Starts background jobs processing.
    *
    * @return `true` if the background jobs were started and `false` if they are
    * already running.
    */
  def startBackgroundJobs(): Boolean
}

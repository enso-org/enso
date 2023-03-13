package org.enso.interpreter.instrument.execution

import java.util.UUID

/** Controls running jobs.
  */
trait JobControlPlane {

  /** Aborts all interruptible jobs.
    */
  def abortAllJobs(): Unit

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

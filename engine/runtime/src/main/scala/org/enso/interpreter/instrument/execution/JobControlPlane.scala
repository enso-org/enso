package org.enso.interpreter.instrument.execution

import java.util.UUID

/**
  * Controls running jobs.
  */
trait JobControlPlane {

  /**
    * Aborts all interruptible jobs.
    */
  def abortAllJobs(): Unit

  /**
    * Aborts all jobs that relates to the specified execution context.
    *
    * @param contextId an identifier of a context
    */
  def abortJobs(contextId: UUID): Unit

}

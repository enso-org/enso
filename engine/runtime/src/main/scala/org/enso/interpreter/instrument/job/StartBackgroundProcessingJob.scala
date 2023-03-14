package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

/** A job responsible for starting background jobs processing. */
final class StartBackgroundProcessingJob()
    extends Job[Unit](
      List(),
      isCancellable         = false,
      mayInterruptIfRunning = false
    ) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit =
    StartBackgroundProcessingJob.startBackgroundJobs()
}

object StartBackgroundProcessingJob {

  /** Start background jobs execution. */
  def startBackgroundJobs()(implicit ctx: RuntimeContext): Unit = {
    val jobsStarted = ctx.jobControlPlane.startBackgroundJobs()
    if (jobsStarted) {
      ctx.executionService.getLogger
        .log(Level.INFO, "Background jobs started.")
      ctx.endpoint.sendToClient(
        Api.Response(Api.BackgroundJobsStartedNotification())
      )
    }
  }

}

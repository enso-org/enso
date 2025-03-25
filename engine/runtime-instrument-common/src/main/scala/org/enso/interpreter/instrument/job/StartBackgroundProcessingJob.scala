package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

/** A job responsible for starting background jobs processing. */
final class StartBackgroundProcessingJob()
    extends Job[Unit](
      List(),
      isCancellable         = false,
      mayInterruptIfRunning = false
    ) {

  /** @inheritdoc */
  override def runImpl(implicit ctx: RuntimeContext): Unit =
    StartBackgroundProcessingJob.startBackgroundJobs()
}

object StartBackgroundProcessingJob {
  private def logger: org.slf4j.Logger =
    org.slf4j.LoggerFactory.getLogger(getClass)

  /** Start background jobs execution. */
  def startBackgroundJobs()(implicit ctx: RuntimeContext): Unit = {
    val jobsStarted = ctx.jobControlPlane.startBackgroundJobs()
    if (jobsStarted) {
      logger.debug("Background jobs started")
      ctx.endpoint.sendToClient(
        Api.Response(Api.BackgroundJobsStartedNotification())
      )
    }
  }

}

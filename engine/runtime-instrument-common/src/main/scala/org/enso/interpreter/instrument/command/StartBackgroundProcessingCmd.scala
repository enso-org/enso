package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.StartBackgroundProcessingJob
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command requesting to start the background jobs processing.
  *
  * @param maybeRequestId an option with request id
  */
final class StartBackgroundProcessingCmd(
  maybeRequestId: Option[Api.RequestId]
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    StartBackgroundProcessingJob.startBackgroundJobs()
    Future.successful(())
  }

}

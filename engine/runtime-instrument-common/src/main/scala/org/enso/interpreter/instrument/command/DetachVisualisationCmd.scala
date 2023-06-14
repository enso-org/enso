package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.DetachVisualisationJob
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that detaches a visualisation from the expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class DetachVisualisationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.DetachVisualisation
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.locking.acquireContextLock(request.contextId)
    try {
      if (doesContextExist) {
        detachVisualization()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(request.contextId)
    }
  }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def detachVisualization()(implicit
    ctx: RuntimeContext
  ): Future[Unit] =
    ctx.jobProcessor.run(
      new DetachVisualisationJob(
        maybeRequestId,
        request.visualisationId,
        request.expressionId,
        request.contextId,
        Api.VisualisationDetached()
      )
    )

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }
  }

}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.DetachVisualizationJob
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that detaches a visualization from the expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class DetachVisualizationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.DetachVisualization
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val logger        = ctx.executionService.getLogger
    val lockTimestamp = ctx.locking.acquireContextLock(request.contextId)
    try {
      if (doesContextExist) {
        detachVisualization()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(request.contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [DetachVisualizationCmd] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def detachVisualization()(implicit
    ctx: RuntimeContext
  ): Future[Unit] = {
    ctx.endpoint.sendToClient(
      Api.Response(maybeRequestId, Api.VisualizationDetached())
    )
    ctx.jobProcessor.run(
      new DetachVisualizationJob(
        request.visualizationId,
        request.expressionId,
        request.contextId
      )
    )
  }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }
  }

}

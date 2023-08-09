package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{ExecuteJob, UpsertVisualizationJob}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that attaches a visualization to an expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class AttachVisualizationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.AttachVisualization
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val logger        = ctx.executionService.getLogger
    val contextId     = request.visualizationConfig.executionContextId
    val lockTimestamp = ctx.locking.acquireContextLock(contextId)
    try {
      if (doesContextExist) {
        attachVisualization()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [AttachVisualizationCmd] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(
      request.visualizationConfig.executionContextId
    )
  }

  private def attachVisualization()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.endpoint.sendToClient(
      Api.Response(maybeRequestId, Api.VisualizationAttached())
    )
    val maybeFutureExecutable =
      ctx.jobProcessor.run(
        new UpsertVisualizationJob(
          maybeRequestId,
          request.visualizationId,
          request.expressionId,
          request.visualizationConfig
        )
      )

    maybeFutureExecutable.flatMap {
      case None             => Future.successful(())
      case Some(executable) => ctx.jobProcessor.run(ExecuteJob(executable))
    }
  }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    Future {
      reply(
        Api.ContextNotExistError(request.visualizationConfig.executionContextId)
      )
    }
  }

  override def toString: String = {
    "AttachVisualizationCmd(visualizationId: " + request.visualizationId + ",expressionId=" + request.expressionId + ")"
  }

}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{ExecuteJob, UpsertVisualisationJob}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that attaches a visualisation to an expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class AttachVisualisationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.AttachVisualisation
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val logger        = ctx.executionService.getLogger
    val contextId     = request.visualisationConfig.executionContextId
    val lockTimestamp = ctx.locking.acquireContextLock(contextId)
    try {
      if (doesContextExist) {
        attachVisualisation()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [AttachVisualisationCmd] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(
      request.visualisationConfig.executionContextId
    )
  }

  private def attachVisualisation()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.endpoint.sendToClient(
      Api.Response(maybeRequestId, Api.VisualisationAttached())
    )
    val maybeFutureExecutable =
      ctx.jobProcessor.run(
        new UpsertVisualisationJob(
          maybeRequestId,
          request.visualisationId,
          request.expressionId,
          request.visualisationConfig
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
        Api.ContextNotExistError(request.visualisationConfig.executionContextId)
      )
    }
  }

  override def toString: String = {
    "AttachVisualizationCmd(visualizationId: " + request.visualisationId + ",expressionId=" + request.expressionId + ")"
  }

}

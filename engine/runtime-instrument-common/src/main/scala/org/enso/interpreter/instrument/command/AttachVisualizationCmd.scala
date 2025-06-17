package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{ExecuteJob, UpsertVisualizationJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that attaches a visualization to an expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class AttachVisualizationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.AttachVisualization
) extends ContextCmd(
      request.visualizationConfig.executionContextId,
      maybeRequestId
    ) {

  override protected def executeCmd()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    logger.debug(
      "Attach visualization cmd for request id [{}] and visualization id [{}]",
      maybeRequestId.toString,
      request.visualizationId
    )
    val maybeFutureExecutable =
      ctx.jobProcessor.run(
        upsertVisualization(
          maybeRequestId,
          request.visualizationId,
          request.expressionId,
          request.visualizationConfig
        )
      )

    maybeFutureExecutable.flatMap {
      case UpsertVisualizationJob.EmptyStack =>
        Future.successful {
          ctx.endpoint.sendToClient(
            Api.Response(
              maybeRequestId,
              Api.EmptyStackError(
                request.visualizationConfig.executionContextId
              )
            )
          )
        }
      case UpsertVisualizationJob.RequiresExecution(executable) =>
        ctx.endpoint.sendToClient(
          Api.Response(maybeRequestId, Api.VisualizationAttached())
        )
        ctx.jobProcessor.run(ExecuteJob(executable))
      case UpsertVisualizationJob.NoExecution =>
        Future.successful {
          ctx.endpoint.sendToClient(
            Api.Response(maybeRequestId, Api.VisualizationAttached())
          )
        }
    }
  }

  def upsertVisualization(
    maybeRequestId: Option[Api.RequestId],
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    config: Api.VisualizationConfiguration
  ): UpsertVisualizationJob = {
    new UpsertVisualizationJob(
      maybeRequestId,
      visualizationId,
      expressionId,
      config
    )
  }

  override def toString: String = {
    "AttachVisualizationCmd(visualizationId: " + request.visualizationId + ",expressionId=" + request.expressionId + ")"
  }

}

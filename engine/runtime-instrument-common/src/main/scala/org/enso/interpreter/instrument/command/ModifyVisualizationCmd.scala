package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{
  EnsureCompiledJob,
  ExecuteJob,
  Job,
  UpsertVisualizationJob
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ExpressionId

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that modifies a visualization.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class ModifyVisualizationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.ModifyVisualization
) extends ContextCmd(
      request.visualizationConfig.executionContextId,
      maybeRequestId
    ) {

  override protected def executeCmd()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.executionService.getLogger.log(
      Level.FINE,
      "Modify visualization cmd for request id [{}] and visualization id [{}]",
      Array(maybeRequestId, request.visualizationId)
    )
    val existingVisualization = ctx.contextManager.getVisualizationById(
      request.visualizationConfig.executionContextId,
      request.visualizationId
    )
    val visualizationPresent: Option[ExpressionId] =
      existingVisualization.map(_.expressionId).orElse {
        val jobFilter: PartialFunction[Job[_], Option[ExpressionId]] = {
          case upsert: UpsertVisualizationJob
              if upsert.visualizationId == request.visualizationId =>
            Some(upsert.expressionId)
        }
        ctx.jobControlPlane.jobInProgress(jobFilter)
      }
    visualizationPresent match {
      case None =>
        Future {
          ctx.endpoint.sendToClient(
            Api.Response(maybeRequestId, Api.VisualizationNotFound())
          )
        }

      case Some(expressionId) =>
        ctx.endpoint.sendToClient(
          Api.Response(maybeRequestId, Api.VisualizationModified())
        )
        val maybeFutureExecutable =
          ctx.jobProcessor.run(
            new UpsertVisualizationJob(
              maybeRequestId,
              request.visualizationId,
              expressionId,
              request.visualizationConfig
            )
          )
        maybeFutureExecutable flatMap {
          case None =>
            Future.successful(())

          case Some(exec) =>
            for {
              _ <- Future {
                ctx.jobProcessor.run(EnsureCompiledJob(exec.stack))
              }
              _ <- ctx.jobProcessor.run(ExecuteJob(exec))
            } yield ()
        }
    }
  }

  override def toString: String = {
    "ModifyVisualizationCmd(visualizationId: " + request.visualizationId + ")"
  }

}

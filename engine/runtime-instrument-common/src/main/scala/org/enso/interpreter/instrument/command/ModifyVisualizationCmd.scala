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
        modifyVisualization()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [UpsertVisualizationJob] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def modifyVisualization()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val existingVisualization = ctx.contextManager.getVisualizationById(
      request.visualizationConfig.executionContextId,
      request.visualizationId
    )
    val visualizationPresent: Option[ExpressionId] =
      existingVisualization.map(_.expressionId).orElse {
        val jobFilter: PartialFunction[Job[_], Option[ExpressionId]] = {
          case upsert: UpsertVisualizationJob
              if upsert.getVisualizationId() == request.visualizationId =>
            Some(upsert.key)
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

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(
      request.visualizationConfig.executionContextId
    )
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
    "ModifyVisualizationCmd(visualizationId: " + request.visualizationId + ")"
  }

}

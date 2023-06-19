package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{
  EnsureCompiledJob,
  ExecuteJob,
  Job,
  UpsertVisualisationJob
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.ExpressionId

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that modifies a visualisation.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class ModifyVisualisationCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.ModifyVisualisation
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
        modifyVisualisation()
      } else {
        replyWithContextNotExistError()
      }
    } finally {
      ctx.locking.releaseContextLock(contextId)
      logger.log(
        Level.FINEST,
        s"Kept context lock [UpsertVisualisationJob] for ${System.currentTimeMillis() - lockTimestamp} milliseconds"
      )
    }
  }

  private def modifyVisualisation()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val existingVisualisation = ctx.contextManager.getVisualisationById(
      request.visualisationConfig.executionContextId,
      request.visualisationId
    )
    val visualisationPresent: Option[ExpressionId] =
      existingVisualisation.map(_.expressionId).orElse {
        val jobFilter: PartialFunction[Job[_], Option[ExpressionId]] = {
          case upsert: UpsertVisualisationJob
              if upsert.getVisualizationId() == request.visualisationId =>
            Some(upsert.key)
        }
        ctx.jobControlPlane.jobInProgress(jobFilter)
      }
    visualisationPresent match {
      case None =>
        Future {
          ctx.endpoint.sendToClient(
            Api.Response(maybeRequestId, Api.VisualisationNotFound())
          )
        }

      case Some(expressionId) =>
        ctx.endpoint.sendToClient(
          Api.Response(maybeRequestId, Api.VisualisationModified())
        )
        val maybeFutureExecutable =
          ctx.jobProcessor.run(
            new UpsertVisualisationJob(
              maybeRequestId,
              request.visualisationId,
              expressionId,
              request.visualisationConfig
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
      request.visualisationConfig.executionContextId
    )
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
    "ModifyVisualisationCmd(visualizationId: " + request.visualisationId + ")"
  }

}

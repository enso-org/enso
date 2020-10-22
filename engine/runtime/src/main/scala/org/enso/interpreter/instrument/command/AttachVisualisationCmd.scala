package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{
  EnsureCompiledStackJob,
  ExecuteJob,
  UpsertVisualisationJob
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that attaches a visualisation to an expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class AttachVisualisationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.AttachVisualisation
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    if (doesContextExist) {
      attachVisualisation()
    } else {
      replyWithContextNotExistError()
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
    val maybeFutureExecutable =
      ctx.jobProcessor.run(
        new UpsertVisualisationJob(
          maybeRequestId,
          request.visualisationId,
          request.expressionId,
          request.visualisationConfig,
          Api.VisualisationAttached()
        )
      )

    maybeFutureExecutable flatMap {
      case None =>
        Future.successful(())

      case Some(executable) =>
        for {
          _ <- ctx.jobProcessor.run(
            new EnsureCompiledStackJob(executable.stack)
          )
          _ <- ctx.jobProcessor.run(new ExecuteJob(executable))
        } yield ()
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

}

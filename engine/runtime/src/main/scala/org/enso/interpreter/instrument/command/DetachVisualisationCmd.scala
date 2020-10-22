package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
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
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      if (doesContextExist) {
        ctx.contextManager.removeVisualisation(
          request.contextId,
          request.expressionId,
          request.visualisationId
        )
        reply(Api.VisualisationDetached())
      } else {
        reply(Api.ContextNotExistError(request.contextId))
      }
    }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

}

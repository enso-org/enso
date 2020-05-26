package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that detaches a visualisation from the expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class DetachVisualisationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.DetachVisualisation
) extends Command {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.contains(request.contextId)) {
      ctx.contextManager.removeVisualisation(
        request.contextId,
        request.expressionId,
        request.visualisationId
      )
      ctx.endpoint.sendToClient(
        Api.Response(
          maybeRequestId,
          Api.VisualisationDetached()
        )
      )
    } else {
      ctx.endpoint.sendToClient(
        Api.Response(
          maybeRequestId,
          Api.ContextNotExistError(request.contextId)
        )
      )
    }
  }

}

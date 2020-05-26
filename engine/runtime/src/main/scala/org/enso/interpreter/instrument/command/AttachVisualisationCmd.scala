package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that attaches a visualisation to an expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class AttachVisualisationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.AttachVisualisation
) extends BaseVisualisationCmd {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.contains(
          request.visualisationConfig.executionContextId
        )) {
      upsertVisualisation(
        maybeRequestId,
        request.visualisationId,
        request.expressionId,
        request.visualisationConfig,
        Api.VisualisationAttached()
      )
    } else {
      ctx.endpoint.sendToClient(
        Api.Response(
          maybeRequestId,
          Api.ContextNotExistError(
            request.visualisationConfig.executionContextId
          )
        )
      )
    }
  }

}

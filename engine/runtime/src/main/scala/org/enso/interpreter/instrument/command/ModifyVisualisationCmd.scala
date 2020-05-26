package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that modifies a visualisation.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class ModifyVisualisationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.ModifyVisualisation
) extends BaseVisualisationCmd {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    if (ctx.contextManager.contains(
          request.visualisationConfig.executionContextId
        )) {
      val maybeVisualisation = ctx.contextManager.getVisualisationById(
        request.visualisationConfig.executionContextId,
        request.visualisationId
      )
      maybeVisualisation match {
        case None =>
          ctx.endpoint.sendToClient(
            Api.Response(maybeRequestId, Api.VisualisationNotFound())
          )

        case Some(visualisation) =>
          upsertVisualisation(
            maybeRequestId,
            request.visualisationId,
            visualisation.expressionId,
            request.visualisationConfig,
            Api.VisualisationModified()
          )
      }

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

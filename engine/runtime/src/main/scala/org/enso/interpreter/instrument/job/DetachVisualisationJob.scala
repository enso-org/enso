package org.enso.interpreter.instrument.job

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api.{
  ContextId,
  ExpressionId,
  RequestId,
  VisualisationId
}
import org.enso.polyglot.runtime.Runtime.{Api, ApiResponse}

/** A job that detaches a visualisation.
  *
  * @param requestId maybe a request id
  * @param visualisationId an identifier of visualisation
  * @param expressionId an identifier of expression
  * @param contextId an execution context id
  * @param response a response used to reply to a client
  */
class DetachVisualisationJob(
  requestId: Option[RequestId],
  visualisationId: VisualisationId,
  expressionId: ExpressionId,
  contextId: ContextId,
  response: ApiResponse
) extends Job[Unit](List(contextId), true, false) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireContextLock(contextId)
    try {
      ctx.contextManager.removeVisualisation(
        contextId,
        expressionId,
        visualisationId
      )
      ctx.endpoint.sendToClient(Api.Response(requestId, response))
    } finally {
      ctx.locking.releaseContextLock(contextId)
    }
  }
}

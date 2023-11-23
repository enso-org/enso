package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.DetachVisualizationJob
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that detaches a visualization from the expression.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class DetachVisualizationCmd(
  maybeRequestId: Option[RequestId],
  request: Api.DetachVisualization
) extends ContextCmd(request.contextId, maybeRequestId) {

  override protected def executeCmd()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.endpoint.sendToClient(
      Api.Response(maybeRequestId, Api.VisualizationDetached())
    )
    ctx.jobProcessor.run(
      new DetachVisualizationJob(
        request.visualizationId,
        request.expressionId,
        request.contextId
      )
    )
  }

}

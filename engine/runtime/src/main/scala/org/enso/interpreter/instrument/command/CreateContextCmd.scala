package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

/**
  * A command that creates an execution context.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class CreateContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.CreateContextRequest
) extends Command {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.create(request.contextId)
    ctx.endpoint.sendToClient(
      Api.Response(maybeRequestId, Api.CreateContextResponse(request.contextId))
    )
  }

}

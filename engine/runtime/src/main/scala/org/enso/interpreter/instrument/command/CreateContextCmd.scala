package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that creates an execution context.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class CreateContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.CreateContextRequest
) extends Command(maybeRequestId) {

  /** @inheritdoc **/
  override def execute(
    implicit ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.contextManager.create(request.contextId)
      reply(Api.CreateContextResponse(request.contextId))
    }

}

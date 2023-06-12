package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that creates an execution context.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class CreateContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.CreateContextRequest
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.contextManager.create(request.contextId)
      reply(Api.CreateContextResponse(request.contextId))
    }

}

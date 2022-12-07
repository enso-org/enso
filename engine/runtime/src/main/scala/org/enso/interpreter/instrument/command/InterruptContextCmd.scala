package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that forces an interruption of the current execution.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class InterruptContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.InterruptContextRequest
) extends Command(maybeRequestId) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    if (doesContextExist) {
      Future {
        ctx.jobControlPlane.abortJobs(request.contextId)
        reply(Api.InterruptContextResponse(request.contextId))
      }
    } else {
      replyWithContextNotExistError()
    }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def replyWithContextNotExistError()(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      reply(Api.ContextNotExistError(request.contextId))
    }
}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/** A command that destroys the specified execution context.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class DestroyContextCmd(
  maybeRequestId: Option[RequestId],
  request: Api.DestroyContextRequest
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      if (doesContextExist) {
        removeContext()
      } else {
        reply(Api.ContextNotExistError(request.contextId))
      }
    }

  private def doesContextExist(implicit ctx: RuntimeContext): Boolean = {
    ctx.contextManager.contains(request.contextId)
  }

  private def removeContext()(implicit ctx: RuntimeContext): Unit = {
    ctx.jobControlPlane.abortJobs(request.contextId, "destroy context", false)
    val contextLock = ctx.locking.getOrCreateContextLock(request.contextId)
    try {
      ctx.locking.withContextLock(
        contextLock,
        this.getClass,
        () => {
          ctx.contextManager.destroy(request.contextId)
          reply(Api.DestroyContextResponse(request.contextId))
        }
      )
    } finally {
      ctx.locking.removeContextLock(contextLock)
    }
  }

}

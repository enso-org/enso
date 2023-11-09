package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import java.util.logging.Level
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
    val logger = ctx.executionService.getLogger
    ctx.jobControlPlane.abortJobs(request.contextId)
    var lockTimestamp: Long = 0;
    try {
      lockTimestamp = ctx.locking.acquireContextLock(request.contextId)
      ctx.contextManager.destroy(request.contextId)
      reply(Api.DestroyContextResponse(request.contextId))
    } catch {
      case ie: InterruptedException =>
        logger.log(Level.WARNING, "Failed to acquire lock: interrupted", ie)
    } finally {
      logLockRelease(
        logger,
        "context",
        lockTimestamp,
        ctx.locking.removeContextLock(request.contextId)
      )
    }
  }

}

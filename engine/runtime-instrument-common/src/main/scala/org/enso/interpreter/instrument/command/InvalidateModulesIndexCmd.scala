package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

import scala.concurrent.{ExecutionContext, Future}

/** A command that invalidates the modules index.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for invalidation
  */
class InvalidateModulesIndexCmd(
  maybeRequestId: Option[Api.RequestId],
  val request: Api.InvalidateModulesIndexRequest
) extends AsynchronousCommand(maybeRequestId) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val logger = ctx.executionService.getLogger
    val writeCompilationLockTimestamp =
      ctx.locking.acquireWriteCompilationLock()
    try {
      ctx.jobControlPlane.abortAllJobs()
      ctx.executionService.getContext.getTopScope.getModules
        .forEach(_.setIndexed(false))
      reply(Api.InvalidateModulesIndexResponse())
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      logger.log(
        Level.FINEST,
        s"Kept write compilation lock [{0}] for {1} milliseconds.",
        Array(
          getClass.getSimpleName,
          System.currentTimeMillis - writeCompilationLockTimestamp
        )
      )
    }
    Future.successful(())
  }

}

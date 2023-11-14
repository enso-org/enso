package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.ExecutionContext

/** A command that opens a file.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class OpenFileCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.OpenFileRequest
) extends SynchronousCommand(None) {

  /** @inheritdoc */
  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    val logger            = ctx.executionService.getLogger
    val readLockTimestamp = ctx.locking.acquireReadCompilationLock()
    val fileLockTimestamp = ctx.locking.acquireFileLock(request.path)
    try {
      ctx.executionService.setModuleSources(
        request.path,
        request.contents
      )
      ctx.endpoint.sendToClient(
        Api.Response(maybeRequestId, Api.OpenFileResponse)
      )
    } finally {
      ctx.locking.releaseFileLock(request.path)
      logger.log(
        Level.FINEST,
        "Kept file lock [OpenFileCmd] for " + (System.currentTimeMillis - fileLockTimestamp) + " milliseconds"
      )
      ctx.locking.releaseReadCompilationLock()
      logger.log(
        Level.FINEST,
        "Kept read compilation lock [OpenFileCmd] for " + (System.currentTimeMillis - readLockTimestamp) + " milliseconds"
      )

    }
  }
}

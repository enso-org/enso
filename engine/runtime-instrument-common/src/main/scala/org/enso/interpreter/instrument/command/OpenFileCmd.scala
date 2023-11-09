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
    val logger                  = ctx.executionService.getLogger
    var readLockTimestamp: Long = 0
    var fileLockTimestamp: Long = 0
    try {
      readLockTimestamp = ctx.locking.acquireReadCompilationLock()
      fileLockTimestamp = ctx.locking.acquireFileLock(request.path)
      ctx.executionService.setModuleSources(
        request.path,
        request.contents
      )
      ctx.endpoint.sendToClient(
        Api.Response(maybeRequestId, Api.OpenFileResponse)
      )
    } catch {
      case ie: InterruptedException =>
        logger.log(Level.WARNING, "Failed to acquire lock: interrupted", ie)
    } finally {
      if (fileLockTimestamp != 0) {
        ctx.locking.releaseFileLock(request.path)
        logger.log(
          Level.FINEST,
          "Kept file lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis() - fileLockTimestamp
          )
        )
      }
      if (readLockTimestamp != 0) {
        ctx.locking.releaseReadCompilationLock()
        logger.log(
          Level.FINEST,
          "Kept read compilation lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis() - readLockTimestamp
          )
        )
      }
    }
  }
}

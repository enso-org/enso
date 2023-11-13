package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.ExecutionContext

/** A command that closes a file.
  *
  * @param request a request for a service
  */
class CloseFileCmd(request: Api.CloseFileNotification)
    extends SynchronousCommand(None) {

  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    val logger = ctx.executionService.getLogger

    var readLockTimestamp: Long         = 0;
    var fileLockTimestamp: Long         = 0;
    var pendingEditsLockTimestamp: Long = 0;
    try {
      readLockTimestamp         = ctx.locking.acquireReadCompilationLock()
      fileLockTimestamp         = ctx.locking.acquireFileLock(request.path)
      pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()
      ctx.state.pendingEdits.dequeue(request.path)
      ctx.executionService.resetModuleSources(request.path)
    } catch {
      case ie: InterruptedException =>
        logger.log(Level.WARNING, "Failed to acquire lock: interrupted", ie)
    } finally {
      logLockRelease(
        logger,
        "pending edits",
        pendingEditsLockTimestamp,
        ctx.locking.releasePendingEditsLock()
      )
      logLockRelease(
        logger,
        "file",
        fileLockTimestamp,
        ctx.locking.releaseFileLock(request.path)
      )
      logLockRelease(
        logger,
        "read compilation",
        readLockTimestamp,
        ctx.locking.releaseReadCompilationLock()
      )
    }
  }

}

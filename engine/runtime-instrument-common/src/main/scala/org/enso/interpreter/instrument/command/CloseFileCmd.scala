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
      if (pendingEditsLockTimestamp != 0) {
        ctx.locking.releasePendingEditsLock()
        logger.log(
          Level.FINEST,
          "Kept pending edits lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis - pendingEditsLockTimestamp
          )
        )
      }
      if (fileLockTimestamp != 0) {
        ctx.locking.releaseFileLock(request.path)
        logger.log(
          Level.FINEST,
          "Kept file lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis - fileLockTimestamp
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
            System.currentTimeMillis - readLockTimestamp
          )
        )
      }
    }
  }

}

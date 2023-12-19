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
    val logger                    = ctx.executionService.getLogger
    val readLockTimestamp         = ctx.locking.acquireReadCompilationLock()
    val fileLockTimestamp         = ctx.locking.acquireFileLock(request.path)
    val pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()
    try {
      ctx.state.pendingEdits.dequeue(request.path)
      ctx.executionService.resetModuleSources(request.path)
    } finally {
      ctx.locking.releasePendingEditsLock()
      logger.log(
        Level.FINEST,
        "Kept pending edits lock [CloseFileCmd] for " + (System.currentTimeMillis - pendingEditsLockTimestamp) + " milliseconds"
      )
      ctx.locking.releaseFileLock(request.path)
      logger.log(
        Level.FINEST,
        "Kept file lock [CloseFileCmd] for " + (System.currentTimeMillis - fileLockTimestamp) + " milliseconds"
      )
      ctx.locking.releaseReadCompilationLock()
      logger.log(
        Level.FINEST,
        "Kept read compilation lock [CloseFileCmd] for " + (System.currentTimeMillis - readLockTimestamp) + " milliseconds"
      )
    }
  }

}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

/** A command that closes a file.
  *
  * @param request a request for a service
  */
class CloseFileCmd(request: Api.CloseFileNotification)
    extends SynchronousCommand(None) {

  override def executeSynchronously(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireReadCompilationLock()
    ctx.locking.acquireFileLock(request.path)
    ctx.locking.acquirePendingEditsLock()
    try {
      ctx.state.pendingEdits.dequeue(request.path)
      ctx.executionService.resetModuleSources(request.path)
    } finally {
      ctx.locking.releasePendingEditsLock()
      ctx.locking.releaseFileLock(request.path)
      ctx.locking.releaseReadCompilationLock()
    }
  }

}

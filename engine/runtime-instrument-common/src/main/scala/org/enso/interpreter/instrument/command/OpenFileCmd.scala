package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

/** A command that opens a file.
  *
  * @param request a request for a service
  */
class OpenFileCmd(request: Api.OpenFileNotification)
    extends SynchronousCommand(None) {

  /** @inheritdoc */
  override def executeSynchronously(implicit
    ctx: RuntimeContext
  ): Unit = {
    ctx.locking.acquireReadCompilationLock()
    ctx.locking.acquireFileLock(request.path)
    try {
      ctx.executionService.setModuleSources(
        request.path,
        request.contents
      )
    } finally {
      ctx.locking.releaseFileLock(request.path)
      ctx.locking.releaseReadCompilationLock()
    }
  }
}

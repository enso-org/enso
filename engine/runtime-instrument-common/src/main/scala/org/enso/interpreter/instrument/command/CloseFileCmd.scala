package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

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
    ctx.locking.withReadCompilationLock(
      this.getClass,
      () =>
        ctx.locking.withFileLock(
          request.path,
          this.getClass,
          () =>
            ctx.locking.withPendingEditsLock(
              this.getClass,
              () => {
                ctx.state.pendingEdits.dequeue(request.path)
                ctx.executionService.resetModuleSources(request.path)
              }
            )
        )
    )
  }

}

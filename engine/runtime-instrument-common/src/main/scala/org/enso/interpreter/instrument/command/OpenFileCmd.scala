package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

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
    ctx.locking.withReadCompilationLock(
      this.getClass,
      () =>
        ctx.locking.withFileLock(
          request.path,
          this.getClass,
          () => {
            ctx.executionService.setModuleSources(
              request.path,
              request.contents
            )
            ctx.endpoint.sendToClient(
              Api.Response(maybeRequestId, Api.OpenFileResponse)
            )
          }
        )
    )
  }
}

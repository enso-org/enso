package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that closes a file.
  *
  * @param request a request for a service
  */
class CloseFileCmd(request: Api.CloseFileNotification) extends Command(None) {

  /** @inheritdoc **/
  override def execute(
    implicit ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.locking.acquireFileLock(request.path)
      ctx.locking.acquireReadCompilationLock()
      try {
        ctx.executionService.resetModuleSources(request.path)
      } finally {
        ctx.locking.releaseReadCompilationLock()
        ctx.locking.releaseFileLock(request.path)
      }
    }

}

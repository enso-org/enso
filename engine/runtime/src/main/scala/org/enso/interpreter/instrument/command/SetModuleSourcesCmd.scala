package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that sets sources for the module..
  *
  * @param request a request for a service
  */
class SetModuleSourcesCmd(request: Api.SetModuleSourcesNotification)
    extends Command(None) {

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.locking.acquireFileLock(request.path)
      ctx.locking.acquireReadCompilationLock()
      try {
        ctx.executionService.setModuleSources(
          request.path,
          request.contents
        )
      } finally {
        ctx.locking.releaseReadCompilationLock()
        ctx.locking.releaseFileLock(request.path)
      }
    }

}

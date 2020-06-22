package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.RequestId

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that orchestrates renaming of a project name.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RenameProjectCmd(
  maybeRequestId: Option[RequestId],
  request: Api.RenameProject
) extends Command(maybeRequestId) {

  /** @inheritdoc **/
  override def execute(
    implicit ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.locking.acquireWriteCompilationLock()
      try {
        val context = ctx.executionService.getContext
        context.renameProject(request.oldName, request.newName)
        reply(Api.ProjectRenamed())
      } finally {
        ctx.locking.acquireWriteCompilationLock()
      }
    }

}

package org.enso.interpreter.instrument.command

import java.util.logging.Level

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

  /** @inheritdoc */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    Future {
      ctx.locking.acquireWriteCompilationLock()
      try {
        val logger = ctx.executionService.getLogger
        logger.log(
          Level.FINE,
          s"Renaming project [old:${request.oldName},new:${request.newName}]..."
        )
        val context = ctx.executionService.getContext
        context.renameProject(request.oldName, request.newName)
        reply(Api.ProjectRenamed(request.newName))
        logger.log(Level.INFO, s"Project renamed to ${request.newName}")
      } finally {
        ctx.locking.releaseWriteCompilationLock()
      }
    }

}

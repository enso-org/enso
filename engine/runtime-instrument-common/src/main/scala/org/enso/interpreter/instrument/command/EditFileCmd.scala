package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that performs edition of a file.
  *
  * @param request a request for editing
  */
class EditFileCmd(request: Api.EditFileNotification)
    extends AsynchronousCommand(None) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.locking.acquireFileLock(request.path)
    ctx.locking.acquirePendingEditsLock()
    try {
      val edits =
        request.edits.map(edit => PendingEdit.ApplyEdit(edit, request.execute))
      ctx.state.pendingEdits.enqueue(request.path, edits)
      if (request.execute) {
        ctx.jobControlPlane.abortAllJobs()
        ctx.jobProcessor.run(new EnsureCompiledJob(Seq(request.path)))
        executeJobs.foreach(ctx.jobProcessor.run)
      }
      Future.successful(())
    } finally {
      ctx.locking.releasePendingEditsLock()
      ctx.locking.releaseFileLock(request.path)
    }
  }

  private def executeJobs(implicit
    ctx: RuntimeContext
  ): Iterable[ExecuteJob] = {
    ctx.contextManager.getAllContexts
      .collect {
        case (contextId, stack) if stack.nonEmpty =>
          ExecuteJob(contextId, stack.toList)
      }
  }

}

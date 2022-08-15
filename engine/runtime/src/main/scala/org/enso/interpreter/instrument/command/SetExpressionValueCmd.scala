package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that performs expression update.
  *
  * @param request a request for the update
  */
class SetExpressionValueCmd(request: Api.SetExpressionValueNotification)
    extends Command(None) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.locking.acquireFileLock(request.path)
    ctx.locking.acquirePendingEditsLock()
    try {
      val pendingApplyEdits =
        request.edits.map(
          PendingEdit.SetExpressionValue(
            _,
            request.expressionId,
            request.expressionValue
          )
        )
      ctx.state.pendingEdits.enqueue(request.path, pendingApplyEdits)
      ctx.jobControlPlane.abortAllJobs()
      ctx.jobProcessor.run(new EnsureCompiledJob(Seq(request.path)))
      executeJobs.foreach(ctx.jobProcessor.run)
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
          new ExecuteJob(contextId, stack.toList)
      }
  }

}

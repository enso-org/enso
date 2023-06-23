package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.{ExecutionContext, Future}

/** A command that performs expression update.
  *
  * @param request a request for the update
  */
class SetExpressionValueCmd(request: Api.SetExpressionValueNotification)
    extends AsynchronousCommand(None) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val logger                    = ctx.executionService.getLogger
    val fileLockTimestamp         = ctx.locking.acquireFileLock(request.path)
    val pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()
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
      logger.log(
        Level.FINEST,
        "Kept pending edits lock [SetExpressionValueCmd] for " + (System.currentTimeMillis - pendingEditsLockTimestamp) + " milliseconds"
      )
      ctx.locking.releaseFileLock(request.path)
      logger.log(
        Level.FINEST,
        "Kept file lock [SetExpressionValueCmd] for " + (System.currentTimeMillis - fileLockTimestamp) + " milliseconds"
      )

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

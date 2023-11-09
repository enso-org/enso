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
    val logger                          = ctx.executionService.getLogger
    var fileLockTimestamp: Long         = 0
    var pendingEditsLockTimestamp: Long = 0
    try {
      fileLockTimestamp         = ctx.locking.acquireFileLock(request.path)
      pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()

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
    } catch {
      case ie: InterruptedException =>
        logger.log(Level.WARNING, "Failed to acquire lock: interrupted", ie)
        Future.failed(ie)
    } finally {
      if (pendingEditsLockTimestamp != 0) {
        ctx.locking.releasePendingEditsLock()
        logger.log(
          Level.FINEST,
          "Kept pending edits lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis - pendingEditsLockTimestamp
          )
        )
      }
      if (fileLockTimestamp != 0) {
        ctx.locking.releaseFileLock(request.path)
        logger.log(
          Level.FINEST,
          "Kept file lock [{0}] for {1} milliseconds",
          Array(
            getClass.getSimpleName,
            System.currentTimeMillis - fileLockTimestamp
          )
        )
      }

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

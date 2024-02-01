package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level
import scala.concurrent.ExecutionContext

/** A command that performs edition of a file.
  *
  * @param request a request for editing
  */
class EditFileCmd(request: Api.EditFileNotification)
    extends SynchronousCommand(None) {

  /** Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def executeSynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Unit = {
    val logger                    = ctx.executionService.getLogger
    val fileLockTimestamp         = ctx.locking.acquireFileLock(request.path)
    val pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()
    try {
      logger.log(
        Level.FINE,
        "Adding pending file edits: {}",
        request.edits.map(e => (e.range, e.text.length))
      )
      val edits =
        request.edits.map(edit => PendingEdit.ApplyEdit(edit, request.execute))
      ctx.state.pendingEdits.enqueue(request.path, edits)
      if (request.execute) {
        ctx.jobControlPlane.abortAllJobs()
        ctx.jobProcessor.run(new EnsureCompiledJob(Seq(request.path)))
        executeJobs.foreach(ctx.jobProcessor.run)
      }
    } finally {
      ctx.locking.releasePendingEditsLock()
      logger.log(
        Level.FINEST,
        "Kept pending edits lock [EditFileCmd] for " + (System.currentTimeMillis - pendingEditsLockTimestamp) + " milliseconds"
      )
      ctx.locking.releaseFileLock(request.path)
      logger.log(
        Level.FINEST,
        "Kept file lock [EditFileCmd] for " + (System.currentTimeMillis - fileLockTimestamp) + " milliseconds"
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

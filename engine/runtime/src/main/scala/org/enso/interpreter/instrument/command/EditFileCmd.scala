package org.enso.interpreter.instrument.command

import java.util.logging.Level

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that performs edition of a file.
  *
  * @param request a request for editing
  */
class EditFileCmd(request: Api.EditFileNotification) extends Command(None) {

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
      ctx.executionService.getLogger
        .log(Level.FINE, s"EditFileCmd ${request.path}")
      ctx.state.pendingEdits.enqueue(request.path, request.edits)
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
    ctx.contextManager.getAll
      .filter(kv => kv._2.nonEmpty)
      .mapValues(_.toList)
      .map { case (contextId, stack) =>
        new ExecuteJob(contextId, stack, Seq(), sendMethodCallUpdates = false)
      }
  }

}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/** A command that performs reloading the contents of the file.
  *
  * @param request a request for reload
  */
class ReloadFileCmd(request: Api.ReloadFileNotification) extends Command(None) {

  override def execute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    ctx.locking.acquireFileLock(request.path)
    try {
      ctx.jobControlPlane.abortAllJobs()
      ctx.jobProcessor.run(
        new EnsureCompiledJob(Seq(request.path), forceCompilation = true)
      )
      executeJobs.foreach(ctx.jobProcessor.run)
      Future.successful(())
    } finally {
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

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.{ExecutionContext, Future}

/**
  * A command that performs edition of a file.
  *
  * @param request a request for editing
  */
class EditFileCmd(request: Api.EditFileNotification) extends Command(None) {

  /**
    * Executes a request.
    *
    * @param ctx contains suppliers of services to perform a request
    */
  override def execute(
    implicit ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    for {
      _ <- Future { ctx.jobControlPlane.abortAllJobs() }
      _ <- ctx.jobProcessor.run(
        new EnsureCompiledJob(request.path, request.edits)
      )
      _ <- Future.sequence(executeJobs.map(ctx.jobProcessor.run))
    } yield ()
  }

  private def executeJobs(
    implicit ctx: RuntimeContext
  ): Iterable[ExecuteJob] = {
    ctx.contextManager.getAll
      .filter(kv => kv._2.nonEmpty)
      .mapValues(_.toList)
      .map {
        case (contextId, stack) =>
          new ExecuteJob(contextId, stack, Seq())
      }
  }

}

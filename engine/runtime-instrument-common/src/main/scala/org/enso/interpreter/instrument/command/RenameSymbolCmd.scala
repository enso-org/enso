package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{
  EnsureCompiledJob,
  ExecuteJob,
  RefactoringRenameJob
}
import org.enso.polyglot.runtime.Runtime.Api

import java.io.File

import scala.concurrent.{ExecutionContext, Future}

/** A command that orchestrates renaming of a symbol.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RenameSymbolCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.RenameSymbol
) extends AsynchronousCommand(maybeRequestId) {

  /** @inheritdoc */
  override def executeAsynchronously(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] = {
    val moduleFile = ctx.executionService.getContext
      .findModule(request.module)
      .map(module => Seq(new File(module.getPath)))
      .orElseGet(() => Seq())

    val ensureCompiledJob = ctx.jobProcessor.run(
      new EnsureCompiledJob(
        (ctx.state.pendingEdits.files ++ moduleFile).distinct,
        isCancellable = false
      )
    )
    val refactoringRenameJob = ctx.jobProcessor.run(
      new RefactoringRenameJob(
        maybeRequestId,
        request.module,
        request.expressionId,
        request.newName
      )
    )
    for {
      _               <- ensureCompiledJob
      refactoredFiles <- refactoringRenameJob
      _ <-
        if (refactoredFiles.isEmpty) Future.successful(())
        else reExecute(refactoredFiles)
    } yield ()
  }

  private def reExecute(files: Seq[File])(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    for {
      _ <- ctx.jobProcessor.run(new EnsureCompiledJob(files))
      _ <- Future.sequence {
        ctx.contextManager.getAllContexts
          .collect {
            case (contextId, stack) if stack.nonEmpty =>
              ctx.jobProcessor.run(ExecuteJob(contextId, stack.toList))
          }
      }
    } yield ()
}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{
  EnsureCompiledJob,
  ExecuteJob,
  RefactoringRenameJob
}
import org.enso.polyglot.runtime.Runtime.Api

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
    val ensureCompiledJob = ctx.jobProcessor.run(
      new EnsureCompiledJob(
        ctx.state.pendingEdits.files,
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
      _ <- ensureCompiledJob
      _ <- refactoringRenameJob
      _ <- reExecute
    } yield ()
  }

  private def reExecute(implicit
    ctx: RuntimeContext,
    ec: ExecutionContext
  ): Future[Unit] =
    for {
      _ <- Future.sequence {
        ctx.contextManager.getAllContexts.toVector.map {
          case (contextId, stack) =>
            for {
              _ <- ctx.jobProcessor.run(EnsureCompiledJob(stack))
              _ <- ctx.jobProcessor.run(ExecuteJob(contextId, stack.toList))
            } yield ()
        }
      }
    } yield ()
}

package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.interpreter.service.error.ModuleNotFoundForFileException
import org.enso.polyglot.runtime.Runtime.Api

import java.util.logging.Level

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
  ): Future[Unit] =
    for {
      _ <- Future { doRename }
      _ <- reExecute
    } yield ()

  private def doRename(implicit ctx: RuntimeContext): Unit = {
    val logger        = ctx.executionService.getLogger
    val lockTimestamp = ctx.locking.acquireWriteCompilationLock()
    try {
      logger.log(
        Level.FINE,
        s"Renaming symbol [${request.expressionId}]..."
      )
      applyPendingEdits()
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
    logger.log(
      Level.FINE,
      s"Finished renaming symbol [${request.expressionId}] in ${System.currentTimeMillis() - lockTimestamp} milliseconds."
    )
  }

  private def applyPendingEdits()(implicit ctx: RuntimeContext): Unit = {
    val logger = ctx.executionService.getLogger
    ctx.locking.acquirePendingEditsLock()
    try {
      val context = ctx.executionService.getContext
      ctx.state.pendingEdits.dequeueAll.foreach { case (file, edits) =>
        ctx.locking.acquireFileLock(file)
        try {
          val module = context
            .getModuleForFile(file)
            .orElseThrow(() => new ModuleNotFoundForFileException(file))
          ctx.executionService.modifyModuleSources(
            module,
            edits.map(_.edit),
            null
          )
        } catch {
          case ex: Exception =>
            logger.log(
              Level.WARNING,
              "Error when trying to apply remaining pending edits",
              ex
            )
        } finally {
          ctx.locking.releaseFileLock(file)
        }
      }
    } finally {
      ctx.locking.releasePendingEditsLock()
    }
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

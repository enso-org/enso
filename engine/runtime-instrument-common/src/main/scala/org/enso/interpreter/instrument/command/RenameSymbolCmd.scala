package org.enso.interpreter.instrument.command

import org.enso.compiler.core.IR
import org.enso.compiler.refactoring.IRUtils
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.interpreter.service.error.{
  ModuleNotFoundException,
  ModuleNotFoundForFileException
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.refactoring.RenameUtils
import org.enso.refactoring.validation.MethodNameValidation

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
    val logger = ctx.executionService.getLogger
    val writeCompilationLockTimestamp =
      ctx.locking.acquireWriteCompilationLock()
    try {
      logger.log(
        Level.FINE,
        s"Renaming symbol [${request.expressionId}]..."
      )
      // before applying the refactoring we should make sure that there are no
      // pending edits.
      applyPendingEdits()
      applyEdits()
    } catch {
      case _: ModuleNotFoundException =>
        reply(Api.ModuleNotFound(request.module))
      case ex: RenameSymbolCmd.ExpressionNotFound =>
        reply(Api.ExpressionNotFound(ex.expressionId))
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      logger.log(
        Level.FINEST,
        s"Kept write compilation lock [${getClass.getSimpleName}] for ${System
          .currentTimeMillis() - writeCompilationLockTimestamp} milliseconds."
      )
    }
  }

  private def applyEdits()(implicit ctx: RuntimeContext): Unit = {
    val module = ctx.executionService.getContext
      .findModule(request.module)
      .orElseThrow(() => new ModuleNotFoundException(request.module))
    val newName = MethodNameValidation.normalize(request.newName)
    val literal = IRUtils
      .findByExternalId(module.getIr, request.expressionId)
      .flatMap(getLiteral)
      .getOrElse(
        throw new RenameSymbolCmd.ExpressionNotFound(request.expressionId)
      )
    val usages = IRUtils
      .findUsages(module.getIr, literal)
      .getOrElse(Set())
      .flatMap(_.location)
      .map(_.location)
      .toSeq
    val edits =
      RenameUtils.buildEdits(module.getLiteralSource, usages, newName)

    val moduleEdits = Api.ModuleTextEdits(request.module, edits.toVector)
    reply(Api.SymbolRenamed(Vector(moduleEdits), newName))
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

  private def getLiteral(ir: IR): Option[IR.Name.Literal] =
    ir match {
      case literal: IR.Name.Literal => Some(literal)
      case _                        => None
    }
}

object RenameSymbolCmd {

  final private class ExpressionNotFound(val expressionId: IR.ExternalId)
      extends Exception(s"Expression was not found by id [$expressionId].")

  final class NotSupported[A <: IR](val node: Class[A])
      extends Exception(s"Renaming of [$node] is not supported.")
}

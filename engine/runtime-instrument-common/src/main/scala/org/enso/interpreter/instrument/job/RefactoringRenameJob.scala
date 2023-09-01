package org.enso.interpreter.instrument.job

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.Name
import org.enso.compiler.refactoring.IRUtils
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.service.error.ModuleNotFoundException
import org.enso.polyglot.runtime.Runtime.{Api, ApiNotification, ApiResponse}
import org.enso.refactoring.RenameUtils
import org.enso.refactoring.validation.MethodNameValidation
import org.enso.text.editing.EditorOps

import java.io.File
import java.util.UUID
import java.util.logging.Level

/** A job responsible for refactoring renaming operation.
  *
  * @param maybeRequestId the original request id
  * @param moduleName the qualified module name
  * @param expressionId the symbol to rename
  * @param newName the new name of the symbol
  */
final class RefactoringRenameJob(
  maybeRequestId: Option[Api.RequestId],
  moduleName: String,
  expressionId: UUID,
  newName: String
) extends Job[Seq[File]](
      List(),
      isCancellable         = false,
      mayInterruptIfRunning = false
    ) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Seq[File] = {
    val logger                   = ctx.executionService.getLogger
    val compilationLockTimestamp = ctx.locking.acquireReadCompilationLock()
    try {
      logger.log(
        Level.FINE,
        s"Renaming symbol [{0}]...",
        expressionId
      )
      val refactoredFile = applyRefactoringEdits()
      Seq(refactoredFile)
    } catch {
      case _: ModuleNotFoundException =>
        reply(Api.ModuleNotFound(moduleName))
        Seq()
      case ex: RefactoringRenameJob.ExpressionNotFound =>
        reply(
          Api.SymbolRenameFailed(
            Api.SymbolRenameFailed.ExpressionNotFound(ex.expressionId)
          )
        )
        Seq()
      case ex: RefactoringRenameJob.FailedToApplyEdits =>
        reply(
          Api.SymbolRenameFailed(
            Api.SymbolRenameFailed.FailedToApplyEdits(ex.module)
          )
        )
        Seq()
      case ex: RefactoringRenameJob.OperationNotSupported =>
        reply(
          Api.SymbolRenameFailed(
            Api.SymbolRenameFailed.OperationNotSupported(ex.expressionId)
          )
        )
        Seq()
    } finally {
      ctx.locking.releaseReadCompilationLock()
      logger.log(
        Level.FINEST,
        s"Kept read compilation lock [{0}] for {1} milliseconds.",
        Array(
          getClass.getSimpleName,
          System.currentTimeMillis() - compilationLockTimestamp
        )
      )
    }
  }

  private def applyRefactoringEdits()(implicit ctx: RuntimeContext): File = {
    val module = ctx.executionService.getContext
      .findModule(moduleName)
      .orElseThrow(() => new ModuleNotFoundException(moduleName))
    val newSymbolName = MethodNameValidation.normalize(newName)

    val expression = IRUtils
      .findByExternalId(module.getIr, expressionId)
      .getOrElse(
        throw new RefactoringRenameJob.ExpressionNotFound(expressionId)
      )
    val local            = getLiteral(expression)
    val methodDefinition = getMethodDefinition(expression)
    val symbol = local
      .orElse(methodDefinition)
      .getOrElse(
        throw new RefactoringRenameJob.OperationNotSupported(expressionId)
      )

    def localUsages = local.flatMap(IRUtils.findLocalUsages(module.getIr, _))
    def methodDefinitionUsages = methodDefinition.flatMap(
      IRUtils.findModuleMethodUsages(module.getName, module.getIr, _)
    )

    val usages = localUsages
      .orElse(methodDefinitionUsages)
      .getOrElse(Set())
      .concat(Set(symbol))
      .flatMap(_.location)
      .map(_.location)
      .toSeq
    val edits =
      RenameUtils.buildEdits(module.getLiteralSource, usages, newSymbolName)

    val oldVersion =
      ctx.versionCalculator.evalVersion(module.getLiteralSource.toString)
    val newContents =
      EditorOps
        .applyEdits(module.getLiteralSource, edits)
        .getOrElse(
          throw new RefactoringRenameJob.FailedToApplyEdits(moduleName)
        )
    val newVersion = ctx.versionCalculator.evalVersion(newContents.toString)

    val fileEdit = Api.FileEdit(
      new File(module.getPath),
      edits.toVector,
      oldVersion.toHexString,
      newVersion.toHexString
    )
    enqueuePendingEdits(fileEdit)
    notify(fileEdit)
    reply(Api.SymbolRenamed(newSymbolName))

    fileEdit.path
  }

  private def enqueuePendingEdits(fileEdit: Api.FileEdit)(implicit
    ctx: RuntimeContext
  ): Unit = {
    val pendingEditsLockTimestamp = ctx.locking.acquirePendingEditsLock()
    try {
      val pendingEdits =
        fileEdit.edits.map(PendingEdit.ApplyEdit(_, execute = true))
      ctx.state.pendingEdits.enqueue(fileEdit.path, pendingEdits)
    } finally {
      ctx.locking.releasePendingEditsLock()
      ctx.executionService.getLogger.log(
        Level.FINEST,
        s"Kept pending edits lock [{0}] for {1} milliseconds.",
        Array(
          getClass.getSimpleName,
          System.currentTimeMillis() - pendingEditsLockTimestamp
        )
      )
    }
  }

  private def getLiteral(ir: IR): Option[Name.Literal] =
    ir match {
      case literal: Name.Literal => Some(literal)
      case _                     => None
    }

  private def getMethodDefinition(ir: IR): Option[Name] =
    ir match {
      case methodRef: Name.MethodReference if methodRef.typePointer.isEmpty =>
        Some(methodRef.methodName)
      case _ =>
        None
    }

  private def reply(
    payload: ApiResponse
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(Api.Response(maybeRequestId, payload))
  }

  private def notify(
    payload: ApiNotification
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(Api.Response(None, payload))
  }

}

object RefactoringRenameJob {

  final private class ExpressionNotFound(val expressionId: IR.ExternalId)
      extends Exception(s"Expression was not found by id [$expressionId].")

  final private class FailedToApplyEdits(val module: String)
      extends Exception(s"Failed to apply edits to module [$module]")

  final private class OperationNotSupported(val expressionId: IR.ExternalId)
      extends Exception(
        s"Operation not supported for expression [$expressionId]"
      )
}

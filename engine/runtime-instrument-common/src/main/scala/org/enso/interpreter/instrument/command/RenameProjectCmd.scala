package org.enso.interpreter.instrument.command

import java.util.logging.Level
import org.enso.interpreter.instrument.{CacheInvalidation, InstrumentFrame}
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.job.{EnsureCompiledJob, ExecuteJob}
import org.enso.pkg.QualifiedName
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A command that orchestrates renaming of a project name.
  *
  * @param maybeRequestId an option with request id
  * @param request a request for a service
  */
class RenameProjectCmd(
  maybeRequestId: Option[Api.RequestId],
  request: Api.RenameProject
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
    val logger             = ctx.executionService.getLogger
    val writeLockTimestamp = ctx.locking.acquireWriteCompilationLock()
    try {
      logger.log(
        Level.FINE,
        s"Renaming project [old:${request.namespace}.${request.oldName},new:${request.namespace}.${request.newName}]..."
      )
      val packageRepository =
        ctx.executionService.getContext.getPackageRepository
      val mainPackage = packageRepository.getMainProjectPackage
        .getOrElse(throw new RenameProjectCmd.MainProjectPackageNotFound)
      val projectModules =
        packageRepository.getModulesForLibrary(mainPackage.libraryName)

      val oldConfig = mainPackage.getConfig()
      val newConfig = mainPackage
        .reloadConfig()
        .fold(
          cause => throw new RenameProjectCmd.FailedToReloadConfig(cause),
          identity
        )

      projectModules.foreach { module =>
        module.setIndexed(false)
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.SuggestionsDatabaseModuleUpdateNotification(
              module = module.getName.toString,
              actions = Vector(
                Api.SuggestionsDatabaseAction.Clean(module.getName.toString)
              ),
              exports = Vector(),
              updates = Tree.empty
            )
          )
        )
      }

      val context = ctx.executionService.getContext
      context.renameProject(
        request.namespace,
        request.oldName,
        request.newName
      )

      ctx.contextManager.getAllContexts.values.foreach { stack =>
        updateMethodPointers(request.newName, stack)
        clearCache(stack)
      }

      reply(
        Api.ProjectRenamed(
          oldConfig.moduleName,
          newConfig.moduleName,
          newConfig.name
        )
      )
      logger.log(
        Level.INFO,
        s"Project renamed to ${request.namespace}.${request.newName}"
      )
    } catch {
      case ex: RenameProjectCmd.MainProjectPackageNotFound =>
        logger.log(
          Level.SEVERE,
          "Main project package is not found.",
          ex
        )
        reply(Api.ProjectRenameFailed(request.oldName, request.newName))
      case ex: RenameProjectCmd.FailedToReloadConfig =>
        logger.log(
          Level.SEVERE,
          "Failed to reload package config.",
          ex
        )
        reply(Api.ProjectRenameFailed(request.oldName, request.newName))
    } finally {
      ctx.locking.releaseWriteCompilationLock()
      logger.log(
        Level.FINEST,
        "Kept write compilation lock [RenameProjectCmd] for " + (System.currentTimeMillis - writeLockTimestamp) + " milliseconds"
      )

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

  /** Update module name of method pointers in the stack.
    *
    * @param projectName the new project name
    * @param stack the exeution stack
    */
  private def updateMethodPointers(
    projectName: String,
    stack: mutable.Stack[InstrumentFrame]
  ): Unit = {
    stack.mapInPlace {
      case InstrumentFrame(call: Api.StackItem.ExplicitCall, cache, sync) =>
        val moduleName = QualifiedName
          .fromString(call.methodPointer.module)
          .renameProject(projectName)
          .toString
        val typeName = QualifiedName
          .fromString(call.methodPointer.definedOnType)
          .renameProject(projectName)
          .toString
        val methodPointer =
          call.methodPointer.copy(module = moduleName, definedOnType = typeName)
        InstrumentFrame(call.copy(methodPointer = methodPointer), cache, sync)
      case item => item
    }
  }

  private def clearCache(stack: Iterable[InstrumentFrame]): Unit = {
    stack.foreach(_.syncState.clearMethodPointersState())
    CacheInvalidation.run(
      stack,
      CacheInvalidation(
        CacheInvalidation.StackSelector.All,
        CacheInvalidation.Command.InvalidateAll
      )
    )
  }

}

object RenameProjectCmd {

  final private class MainProjectPackageNotFound
      extends Exception("Main project package is not found.")

  final private class FailedToReloadConfig(cause: Throwable)
      extends Exception("Failed to reload config", cause)
}

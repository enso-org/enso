package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/**
  * A command that edits a file.
  *
  * == Caching ==
  *
  * Compute invalidated external ids by applying the text edits to the
  * changeset. Invalidated ids are removed from all stack frames.
  *
  * @param request a request for a service
  */
class EditFileCmd(request: Api.EditFileNotification)
    extends Command
    with ProgramExecutionSupport {

  /** @inheritdoc **/
  override def execute(implicit ctx: RuntimeContext): Unit = {
    val changesetOpt = ctx.executionService
      .modifyModuleSources(request.path, request.edits.asJava)
      .toScala
    val invalidateExpressionsCommand = changesetOpt.map { changeset =>
      CacheInvalidation.Command.InvalidateKeys(
        request.edits.flatMap(changeset.compute)
      )
    }
    val invalidateStaleCommand = changesetOpt.map { changeset =>
      val scopeIds = ctx.executionService.getContext.getCompiler
        .parseMeta(changeset.source.toString)
        .map(_._2)
      CacheInvalidation.Command.InvalidateStale(scopeIds)
    }
    val cacheInvalidationCommands =
      (invalidateExpressionsCommand.toSeq ++ invalidateStaleCommand.toSeq)
        .map(
          CacheInvalidation(
            CacheInvalidation.StackSelector.All,
            _,
            Set(CacheInvalidation.IndexSelector.All)
          )
        )
    withContext(executeAll(cacheInvalidationCommands))
  }

  private def executeAll(
    invalidationCommands: Iterable[CacheInvalidation]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.getAll
      .filter(kv => kv._2.nonEmpty)
      .mapValues(_.toList)
      .foreach {
        case (contextId, stack) =>
          CacheInvalidation.runAll(stack, invalidationCommands)
          runProgram(contextId, stack)
      }
  }
}

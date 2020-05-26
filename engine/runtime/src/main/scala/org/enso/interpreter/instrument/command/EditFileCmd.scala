package org.enso.interpreter.instrument.command

import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.polyglot.runtime.Runtime.Api

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/**
  * A command that edits a file.
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
    val invalidateExpressions = changesetOpt.map { changeset =>
      CacheInvalidation.InvalidateKeys(request.edits.flatMap(changeset.compute))
    }
    val invalidateStale = changesetOpt.map { changeset =>
      val scopeIds = ctx.executionService.getContext.getCompiler
        .parseMeta(changeset.source.toString)
        .map(_._2)
      CacheInvalidation.InvalidateStale(scopeIds)
    }
    withContext(
      executeAll(invalidateExpressions.toSeq ++ invalidateStale.toSeq)
    )
  }

  private def executeAll(
    invalidationRules: Iterable[CacheInvalidation]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.getAll
      .filter(kv => kv._2.nonEmpty)
      .mapValues(_.toList)
      .foreach {
        case (contextId, stack) =>
          CacheInvalidation.run(stack, invalidationRules)
          runProgram(contextId, stack)
      }
  }
}

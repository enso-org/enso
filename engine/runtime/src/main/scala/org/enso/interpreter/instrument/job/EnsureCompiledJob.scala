package org.enso.interpreter.instrument.job

import java.io.File
import java.util.Optional

import org.enso.compiler.context.{Changeset, SuggestionBuilder}
import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.buffer.Rope
import org.enso.text.editing.model.TextEdit

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/**
  * A job that ensures that specified files are compiled.
  *
  * @param files a files to compile
  */
class EnsureCompiledJob(protected val files: List[File])
    extends Job[Unit](List.empty, true, false) {

  /**
    * Create a job that ensures that a files is compiled after applying the edits.
    *
    * @param file a file to compile
    */
  def this(file: File, edits: Seq[TextEdit]) = {
    this(List(file))
    EnsureCompiledJob.enqueueEdits(file, edits)
  }

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireWriteCompilationLock()
    try {
      ensureCompiled(files)
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
  }

  /** Run the compilation and invalidation logic.
    *
    * @param files the list of files to compile
    * @param ctx the runtime context
    */
  protected def ensureCompiled(
    files: Iterable[File]
  )(implicit ctx: RuntimeContext): Unit = {
    files.foreach { file =>
      compile(file).foreach { module =>
        applyEdits(file).ifPresent {
          case (changeset, edits) =>
            runInvalidationCommands(
              EnsureCompiledJob.buildCacheInvalidationCommands(changeset, edits)
            )
            val removedSuggestions = SuggestionBuilder(changeset.source)
              .build(module.getName.toString, module.getIr)
            compile(module)
            val addedSuggestions =
              SuggestionBuilder(changeset.applyEdits(edits))
                .build(module.getName.toString, module.getIr)
            sendSuggestionsNotifications(
              removedSuggestions diff addedSuggestions,
              addedSuggestions diff removedSuggestions
            )

        }
      }
    }
  }

  private def compile(
    file: File
  )(implicit ctx: RuntimeContext): Option[Module] = {
    ctx.executionService.getContext
      .getModuleForFile(file)
      .map(compile(_))
      .toScala
  }

  private def compile(module: Module)(implicit ctx: RuntimeContext): Module =
    module.parseScope(ctx.executionService.getContext).getModule

  private def applyEdits(
    file: File
  )(implicit
    ctx: RuntimeContext
  ): Optional[(Changeset[Rope], Seq[TextEdit])] = {
    ctx.locking.acquireFileLock(file)
    ctx.locking.acquireReadCompilationLock()
    try {
      val edits = EnsureCompiledJob.dequeueEdits(file)
      if (edits.nonEmpty) {
        ctx.executionService
          .modifyModuleSources(file, edits.asJava)
          .map(_ -> edits)
      } else {
        Optional.empty()
      }
    } finally {
      ctx.locking.releaseReadCompilationLock()
      ctx.locking.releaseFileLock(file)
    }
  }

  private def runInvalidationCommands(
    invalidationCommands: Iterable[CacheInvalidation]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.getAll.valuesIterator
      .collect {
        case stack if stack.nonEmpty =>
          CacheInvalidation.runAll(stack, invalidationCommands)
      }
  }

  private def sendSuggestionsNotifications(
    removed: Seq[Suggestion],
    added: Seq[Suggestion]
  )(implicit ctx: RuntimeContext): Unit =
    if (added.nonEmpty || removed.nonEmpty) {
      ctx.endpoint.sendToClient(
        Api.Response(
          Api.SuggestionsDatabaseUpdateNotification(
            removed.map(Api.SuggestionsDatabaseUpdate.Remove) :++
            added.map(Api.SuggestionsDatabaseUpdate.Add)
          )
        )
      )
    }

}

object EnsureCompiledJob {

  private val unappliedEdits =
    new TrieMap[File, Seq[TextEdit]]()

  private def dequeueEdits(file: File): Seq[TextEdit] =
    unappliedEdits.remove(file).getOrElse(Seq())

  private def enqueueEdits(file: File, edits: Seq[TextEdit]): Unit =
    unappliedEdits.updateWith(file) {
      case Some(v) => Some(v :++ edits)
      case None    => Some(edits)
    }

  private def buildCacheInvalidationCommands(
    changeset: Changeset[Rope],
    edits: Seq[TextEdit]
  )(implicit ctx: RuntimeContext): Seq[CacheInvalidation] = {
    val invalidateExpressionsCommand =
      CacheInvalidation.Command.InvalidateKeys(changeset.compute(edits))
    val scopeIds = ctx.executionService.getContext.getCompiler
      .parseMeta(changeset.source.toString)
      .map(_._2)
    val invalidateStaleCommand =
      CacheInvalidation.Command.InvalidateStale(scopeIds)
    Seq(invalidateExpressionsCommand, invalidateStaleCommand).map(
      CacheInvalidation(
        CacheInvalidation.StackSelector.All,
        _,
        Set(CacheInvalidation.IndexSelector.All)
      )
    )
  }
}

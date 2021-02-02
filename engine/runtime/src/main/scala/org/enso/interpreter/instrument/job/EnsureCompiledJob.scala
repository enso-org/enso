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
    * Create a job ensuring that files are compiled after applying the edits.
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

  /**
    * Run the compilation and invalidation logic.
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
            val moduleName = module.getName.toString
            runInvalidationCommands(
              buildCacheInvalidationCommands(changeset, edits)
            )
            if (module.isIndexed) {
              val removedSuggestions = SuggestionBuilder(changeset.source)
                .build(moduleName, module.getIr)
              compile(module)
              val addedSuggestions =
                SuggestionBuilder(changeset.applyEdits(edits))
                  .build(moduleName, module.getIr)
              sendSuggestionsUpdateNotification(
                removedSuggestions diff addedSuggestions,
                addedSuggestions diff removedSuggestions
              )
            } else {
              val addedSuggestions =
                SuggestionBuilder(changeset.applyEdits(edits))
                  .build(moduleName, module.getIr)
              sendReIndexNotification(moduleName, addedSuggestions)
              module.setIndexed(true)
            }
        }
      }
    }
  }

  /**
    * Compile the file.
    *
    * @param file the file path to compile
    * @param ctx the runtime context
    * @return the compiled module
    */
  private def compile(
    file: File
  )(implicit ctx: RuntimeContext): Option[Module] = {
    ctx.executionService.getContext
      .getModuleForFile(file)
      .map(compile(_))
      .toScala
  }

  /**
    * Compile the module.
    *
    * @param module the module to compile.
    * @param ctx the runtime context
    * @return the compiled module
    */
  private def compile(module: Module)(implicit ctx: RuntimeContext): Module =
    module.parseScope(ctx.executionService.getContext).getModule

  /**
    * Apply pending edits to the file.
    *
    * @param file the file to apply edits to
    * @param ctx the runtime context
    * @return the [[Changeset]] object and the list of applied edits
    */
  private def applyEdits(
    file: File
  )(implicit
    ctx: RuntimeContext
  ): Optional[(Changeset[Rope], Seq[TextEdit])] = {
    ctx.locking.acquireFileLock(file)
    ctx.locking.acquireReadCompilationLock()
    try {
      val edits = EnsureCompiledJob.dequeueEdits(file)
      ctx.executionService
        .modifyModuleSources(file, edits.asJava)
        .map(_ -> edits)
    } finally {
      ctx.locking.releaseReadCompilationLock()
      ctx.locking.releaseFileLock(file)
    }
  }

  /**
    * Create cache invalidation commands after applying the edits.
    *
    * @param changeset the [[Changeset]] object capturing the previous version
    * of IR
    * @param edits the list of applied edits
    * @param ctx the runtime context
    * @return the list of cache invalidation commands
    */
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

  /**
    * Run the invalidation commands.
    *
    * @param invalidationCommands the invalidation command to run
    * @param ctx the runtime context
    */
  private def runInvalidationCommands(
    invalidationCommands: Iterable[CacheInvalidation]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.getAll.valuesIterator
      .collect {
        case stack if stack.nonEmpty =>
          CacheInvalidation.runAll(stack, invalidationCommands)
      }
  }

  /**
    * Send notification about the suggestions database updates.
    *
    * @param removed the list of suggestions to remove
    * @param added the list of suggestions to add
    * @param ctx the runtime context
    */
  private def sendSuggestionsUpdateNotification(
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

  /**
    * Send notification about the re-indexed module updates.
    *
    * @param moduleName the name of re-indexed module
    * @param added the list of suggestions to add
    * @param ctx the runtime context
    */
  private def sendReIndexNotification(
    moduleName: String,
    added: Seq[Suggestion]
  )(implicit ctx: RuntimeContext): Unit =
    ctx.endpoint.sendToClient(
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          added.map(Api.SuggestionsDatabaseUpdate.Add)
        )
      )
    )
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
}

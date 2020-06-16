package org.enso.interpreter.instrument.job

import java.io.File

import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
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
    * Ensures that a files is compiled after applying the edits
    *
    * @param file a file to compile
    */
  def this(file: File, edits: Seq[TextEdit]) = {
    this(List(file))
    EnsureCompiledJob.enqueueEdits(file, edits)
  }

  /** @inheritdoc **/
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireWriteCompilationLock()
    try {
      val modules = files.flatMap(compile)
      runInvalidation(files)
      modules.foreach(compile)
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
  }

  protected def runInvalidation(
    files: Iterable[File]
  )(implicit ctx: RuntimeContext): Unit =
    runInvalidationCommands {
      files.flatMap { file =>
        applyEdits(file, EnsureCompiledJob.dequeueEdits(file))
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

  private def applyEdits(file: File, edits: Seq[TextEdit])(
    implicit ctx: RuntimeContext
  ): Iterable[CacheInvalidation] = {
    ctx.locking.acquireFileLock(file)
    ctx.locking.acquireReadCompilationLock()
    try {
      val changesetOpt =
        ctx.executionService
          .modifyModuleSources(file, edits.asJava)
          .toScala
      val invalidateExpressionsCommand = changesetOpt.map { changeset =>
        CacheInvalidation.Command.InvalidateKeys(
          changeset.compute(edits)
        )
      }
      val invalidateStaleCommand = changesetOpt.map { changeset =>
        val scopeIds = ctx.executionService.getContext.getCompiler
          .parseMeta(changeset.source.toString)
          .map(_._2)
        CacheInvalidation.Command.InvalidateStale(scopeIds)
      }
      (invalidateExpressionsCommand.toSeq ++ invalidateStaleCommand.toSeq)
        .map(
          CacheInvalidation(
            CacheInvalidation.StackSelector.All,
            _,
            Set(CacheInvalidation.IndexSelector.All)
          )
        )
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

package org.enso.interpreter.instrument.job

import java.io.{File, IOException}
import java.util.logging.Level

import org.enso.compiler.context.{Changeset, SuggestionBuilder}
import org.enso.compiler.phase.ImportResolver
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
class EnsureCompiledJob(protected val files: Iterable[File])
    extends Job[Unit](List.empty, true, false) {

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquireWriteCompilationLock()
    try {
      val modules = ensureCompiledFiles(files)
      ensureCompiledScope()
      ensureCompiledImports(modules)
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
  }

  /**
    * Run the scheduled compilation and invalidation logic.
    *
    * @param files the list of files to compile.
    * @param ctx the runtime context
    */
  protected def ensureCompiledFiles(
    files: Iterable[File]
  )(implicit ctx: RuntimeContext): Iterable[Module] = {
    for {
      file   <- files
      module <- compile(file)
    } yield {
      val changeset = applyEdits(file)
      compile(module)
      runInvalidationCommands(
        buildCacheInvalidationCommands(changeset, module.getLiteralSource)
      )
      analyzeModule(module, changeset)
      module
    }
  }

  /**
    * Compile the imported modules and send the suggestion updates.
    *
    * @param modules the list of modules to analyze.
    * @param ctx the runtime context
    */
  protected def ensureCompiledImports(
    modules: Iterable[Module]
  )(implicit ctx: RuntimeContext): Unit = {
    modules.foreach { module =>
      compile(module)
      val importedModules =
        new ImportResolver(ctx.executionService.getContext.getCompiler)
          .mapImports(module)
          .filter(_.getName != module.getName)
      ctx.executionService.getLogger.finest(
        s"Module ${module.getName} imports ${importedModules.map(_.getName)}"
      )
      importedModules.foreach(analyzeImport)
    }
  }

  /**
    * Compile all modules in the scope and send the extracted suggestions.
    *
    * @param ctx the runtime context
    */
  protected def ensureCompiledScope()(implicit ctx: RuntimeContext): Unit = {
    val modulesInScope =
      ctx.executionService.getContext.getTopScope.getModules.asScala
    ctx.executionService.getLogger
      .finest(s"Modules in scope: ${modulesInScope.map(_.getName)}")
    val updates = modulesInScope.flatMap { module =>
      compile(module)
      analyzeModuleInScope(module)
    }
    sendIndexUpdateNotification(
      Api.SuggestionsDatabaseIndexUpdateNotification(updates)
    )
  }

  private def analyzeImport(
    module: Module
  )(implicit ctx: RuntimeContext): Unit = {
    if (!module.isIndexed && module.getLiteralSource != null) {
      ctx.executionService.getLogger
        .finest(s"Analyzing imported ${module.getName}")
      val moduleName = module.getName.toString
      val addedSuggestions = SuggestionBuilder(module.getLiteralSource)
        .build(module.getName.toString, module.getIr)
        .filter(isSuggestionGlobal)
      sendReIndexNotification(moduleName, addedSuggestions)
      module.setIndexed(true)
    }
  }

  private def analyzeModuleInScope(module: Module)(implicit
    ctx: RuntimeContext
  ): Option[Api.IndexedModule] = {
    try module.getSource
    catch {
      case e: IOException =>
        ctx.executionService.getLogger.log(
          Level.SEVERE,
          s"Failed to get module source to analyze ${module.getName}",
          e
        )
    }
    if (
      !module.isIndexed &&
      module.getLiteralSource != null &&
      module.getPath != null
    ) {
      ctx.executionService.getLogger
        .finest(s"Analyzing module in scope ${module.getName}")
      val moduleName = module.getName.toString
      val addedSuggestions = SuggestionBuilder(module.getLiteralSource)
        .build(moduleName, module.getIr)
        .filter(isSuggestionGlobal)
      module.setIndexed(true)
      Some(
        Api.IndexedModule(
          new File(module.getPath),
          module.getSource.getCharacters.toString,
          addedSuggestions.map(Api.SuggestionsDatabaseUpdate.Add)
        )
      )
    } else {
      None
    }
  }

  private def analyzeModule(
    module: Module,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    val moduleName = module.getName.toString
    if (module.isIndexed) {
      ctx.executionService.getLogger
        .finest(s"Analyzing indexed module ${module.getName}")
      val removedSuggestions = SuggestionBuilder(changeset.source)
        .build(moduleName, changeset.ir)
      val addedSuggestions =
        SuggestionBuilder(module.getLiteralSource)
          .build(moduleName, module.getIr)
      sendSuggestionsUpdateNotification(
        removedSuggestions diff addedSuggestions,
        addedSuggestions diff removedSuggestions
      )
    } else {
      ctx.executionService.getLogger
        .finest(s"Analyzing not-indexed module ${module.getName}")
      val addedSuggestions =
        SuggestionBuilder(module.getLiteralSource)
          .build(moduleName, module.getIr)
      sendReIndexNotification(moduleName, addedSuggestions)
      module.setIndexed(true)
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
  private def compile(
    module: Module
  )(implicit ctx: RuntimeContext): Module = {
    val prevStage = module.getCompilationStage
    module.compileScope(ctx.executionService.getContext).getModule
    if (prevStage != module.getCompilationStage) {
      ctx.executionService.getLogger.finer(
        s"Compiled ${module.getName} $prevStage->${module.getCompilationStage}"
      )
    }
    module
  }

  /**
    * Apply pending edits to the file.
    *
    * @param file the file to apply edits to
    * @param ctx the runtime context
    * @return the [[Changeset]] after applying the edits to the source
    */
  private def applyEdits(
    file: File
  )(implicit ctx: RuntimeContext): Changeset[Rope] = {
    ctx.locking.acquireFileLock(file)
    ctx.locking.acquireReadCompilationLock()
    try {
      val edits = EnsureCompiledJob.dequeueEdits(file)
      val suggestionBuilder = ctx.executionService
        .modifyModuleSources(file, edits.asJava)
      suggestionBuilder.build(edits)
    } finally {
      ctx.locking.releaseReadCompilationLock()
      ctx.locking.releaseFileLock(file)
    }
  }

  /**
    * Create cache invalidation commands after applying the edits.
    *
    * @param changeset the [[Changeset]] object capturing the previous
    * version of IR
    * @param ctx the runtime context
    * @return the list of cache invalidation commands
    */
  private def buildCacheInvalidationCommands(
    changeset: Changeset[Rope],
    source: Rope
  )(implicit ctx: RuntimeContext): Seq[CacheInvalidation] = {
    val invalidateExpressionsCommand =
      CacheInvalidation.Command.InvalidateKeys(changeset.invalidated)
    val scopeIds = ctx.executionService.getContext.getCompiler
      .parseMeta(source.toString)
      .map(_._2)
    val invalidateStaleCommand =
      CacheInvalidation.Command.InvalidateStale(scopeIds)
    Seq(
      CacheInvalidation(
        CacheInvalidation.StackSelector.All,
        invalidateExpressionsCommand,
        Set(CacheInvalidation.IndexSelector.Weights)
      ),
      CacheInvalidation(
        CacheInvalidation.StackSelector.All,
        invalidateStaleCommand,
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
    ctx.contextManager.getAll.values
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
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.endpoint.sendToClient(
      Api.Response(
        Api.SuggestionsDatabaseReIndexNotification(
          moduleName,
          added.map(Api.SuggestionsDatabaseUpdate.Add)
        )
      )
    )
  }

  private def sendIndexUpdateNotification(
    msg: Api.SuggestionsDatabaseIndexUpdateNotification
  )(implicit
    ctx: RuntimeContext
  ): Unit = {
    if (msg.updates.nonEmpty) {
      ctx.endpoint.sendToClient(Api.Response(msg))
    }
  }

  private def isSuggestionGlobal(suggestion: Suggestion): Boolean =
    suggestion match {
      case _: Suggestion.Atom     => true
      case _: Suggestion.Method   => true
      case _: Suggestion.Function => false
      case _: Suggestion.Local    => false
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

  /**
    * Create a job ensuring that files are compiled.
    *
    * @param files the list of files to compile
    * @return a new job
    */
  def apply(files: Iterable[File]): EnsureCompiledJob = {
    new EnsureCompiledJob(files)
  }

  /**
    * Create a job ensuring that files are compiled after applying the edits.
    *
    * @param file a file to compile
    * @param edits the list of edits to apply
    * @return a new job
    */
  def apply(file: File, edits: Seq[TextEdit]): EnsureCompiledJob = {
    EnsureCompiledJob.enqueueEdits(file, edits)
    EnsureCompiledJob(List(file))
  }

  /**
    * Create a job ensuring that modules are compiled.
    *
    * @param modules a list of modules to compile
    * @return a new job
    */
  def apply(
    modules: Iterable[Module]
  )(implicit ctx: RuntimeContext): EnsureCompiledJob = {
    val files = modules.flatMap { module =>
      val file = Option(module.getPath).map(new File(_))
      if (file.isEmpty) {
        ctx.executionService.getLogger
          .severe(s"Failed to get file for module ${module.getName}")
      }
      file
    }
    new EnsureCompiledJob(files)
  }
}

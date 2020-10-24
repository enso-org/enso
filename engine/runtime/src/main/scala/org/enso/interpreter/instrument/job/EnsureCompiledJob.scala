package org.enso.interpreter.instrument.job

import java.io.{File, IOException}
import java.util.logging.Level

import cats.implicits._
import org.enso.compiler.context.{Changeset, ModuleContext, SuggestionBuilder}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.GatherDiagnostics
import org.enso.compiler.phase.ImportResolver
import org.enso.interpreter.instrument.CacheInvalidation
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.Suggestion
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.buffer.Rope
import org.enso.text.editing.model.{Position, Range, TextEdit}

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/** A job that ensures that specified files are compiled.
  *
  * @param files a files to compile
  */
class EnsureCompiledJob(protected val files: Iterable[File])
    extends Job[EnsureCompiledJob.CompilationStatus](List.empty, true, false) {

  import EnsureCompiledJob.CompilationStatus

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): CompilationStatus = {
    ctx.locking.acquireWriteCompilationLock()
    try {
      val modules = files.flatMap { file =>
        ctx.executionService.getContext.getModuleForFile(file).toScala
      }
      ensureIndexedModules(modules)
      ensureIndexedImports(modules)
      ensureCompiledScope()
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
  }

  /** Run the scheduled compilation and invalidation logic, and send the
    * suggestion updates.
    *
    * @param modules the list of modules to compile.
    * @param ctx the runtime context
    */
  protected def ensureIndexedModules(
    modules: Iterable[Module]
  )(implicit ctx: RuntimeContext): Unit = {
    modules
      .foreach { module =>
        compile(module)
        val changeset = applyEdits(new File(module.getPath))
        compile(module).foreach { module =>
          runInvalidationCommands(
            buildCacheInvalidationCommands(changeset, module.getLiteralSource)
          )
          analyzeModule(module, changeset)
        }
      }
  }

  /** Compile the imported modules and send the suggestion updates.
    *
    * @param modules the list of modules to analyze.
    * @param ctx the runtime context
    */
  protected def ensureIndexedImports(
    modules: Iterable[Module]
  )(implicit ctx: RuntimeContext): Unit = {
    modules.foreach { module =>
      compile(module).foreach { module =>
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
  }

  /** Compile all modules in the scope and send the extracted suggestions.
    *
    * @param ctx the runtime context
    */
  protected def ensureCompiledScope()(implicit
    ctx: RuntimeContext
  ): CompilationStatus = {
    val modulesInScope =
      ctx.executionService.getContext.getTopScope.getModules.asScala
    ctx.executionService.getLogger
      .finest(s"Modules in scope: ${modulesInScope.map(_.getName)}")
    modulesInScope
      .map { module =>
        compile(module) match {
          case Left(err) =>
            ctx.executionService.getLogger
              .log(Level.SEVERE, s"Compilation error in ${module.getPath}", err)
            sendFailureUpdate(
              Api.ExecutionResult.Failure(
                err.getMessage,
                Option(module.getPath).map(new File(_))
              )
            )
            CompilationStatus.Failure
          case Right(module) =>
            analyzeModuleInScope(module)
            runCompilationDiagnostics(module)
        }
      }
      .maxOption
      .getOrElse(CompilationStatus.Success)
  }

  private def analyzeImport(
    module: Module
  )(implicit ctx: RuntimeContext): Unit = {
    if (
      !module.isIndexed &&
      module.getLiteralSource != null &&
      module.getPath != null
    ) {
      ctx.executionService.getLogger
        .finest(s"Analyzing imported ${module.getName}")
      val moduleName = module.getName.toString
      val addedSuggestions = SuggestionBuilder(module.getLiteralSource)
        .build(module.getName.toString, module.getIr)
        .filter(isSuggestionGlobal)
      val update = Api.SuggestionsDatabaseModuleUpdateNotification(
        new File(module.getPath),
        module.getLiteralSource.toString,
        Api.SuggestionsDatabaseUpdate.Clean(moduleName) +:
        addedSuggestions.map(Api.SuggestionsDatabaseUpdate.Add)
      )
      sendModuleUpdate(update)
      module.setIndexed(true)
    }
  }

  private def analyzeModuleInScope(module: Module)(implicit
    ctx: RuntimeContext
  ): Unit = {
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
      val update = Api.SuggestionsDatabaseModuleUpdateNotification(
        new File(module.getPath),
        module.getLiteralSource.toString,
        Api.SuggestionsDatabaseUpdate.Clean(moduleName) +:
        addedSuggestions.map(Api.SuggestionsDatabaseUpdate.Add)
      )
      sendModuleUpdate(update)
      module.setIndexed(true)
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
      val update = Api.SuggestionsDatabaseModuleUpdateNotification(
        new File(module.getPath),
        module.getLiteralSource.toString,
        removedSuggestions
          .diff(addedSuggestions)
          .map(Api.SuggestionsDatabaseUpdate.Remove) :++
        addedSuggestions
          .diff(removedSuggestions)
          .map(Api.SuggestionsDatabaseUpdate.Add)
      )
      sendModuleUpdate(update)
    } else {
      ctx.executionService.getLogger
        .finest(s"Analyzing not-indexed module ${module.getName}")
      val addedSuggestions =
        SuggestionBuilder(module.getLiteralSource)
          .build(moduleName, module.getIr)
      val update = Api.SuggestionsDatabaseModuleUpdateNotification(
        new File(module.getPath),
        module.getLiteralSource.toString,
        Api.SuggestionsDatabaseUpdate.Clean(moduleName) +:
        addedSuggestions.map(Api.SuggestionsDatabaseUpdate.Add)
      )
      sendModuleUpdate(update)
      module.setIndexed(true)
    }
  }

  /** Extract compilation diagnostics from the module and send the diagnostic
    * updates.
    *
    * @param module the module to analyze
    * @param ctx the runtime context
    * @return the compilation outcome
    */
  private def runCompilationDiagnostics(module: Module)(implicit
    ctx: RuntimeContext
  ): CompilationStatus = {
    val errors = GatherDiagnostics
      .runModule(module.getIr, ModuleContext(module))
      .unsafeGetMetadata(
        GatherDiagnostics,
        "No diagnostics metadata right after the gathering pass."
      )
      .diagnostics
    val diagnostics = errors.collect {
      case warn: IR.Warning =>
        createDiagnostic(Api.DiagnosticType.Warning(), module, warn)
      case error: IR.Error =>
        createDiagnostic(Api.DiagnosticType.Error(), module, error)
    }
    sendDiagnosticUpdates(diagnostics)
    getCompilationStatus(diagnostics)
  }

  /** Create Api diagnostic message from the `IR` node.
    *
    * @param kind the diagnostic type
    * @param module the module to analyze
    * @param diagnostic the diagnostic `IR` node
    * @return the diagnostic message
    */
  private def createDiagnostic(
    kind: Api.DiagnosticType,
    module: Module,
    diagnostic: IR.Diagnostic
  ): Api.ExecutionResult.Diagnostic = {
    val fileOpt = Option(module.getPath).map(new File(_))
    val locationOpt =
      diagnostic.location.map { loc =>
        val section = module.getSource.createSection(
          loc.location.start,
          loc.location.length
        )
        Range(
          Position(section.getStartLine - 1, section.getStartColumn - 1),
          Position(section.getEndLine - 1, section.getEndColumn)
        )
      }
    Api.ExecutionResult.Diagnostic(
      kind,
      diagnostic.message,
      fileOpt,
      locationOpt,
      Vector()
    )
  }

  /** Compile the module.
    *
    * @param module the module to compile.
    * @param ctx the runtime context
    * @return the compiled module
    */
  private def compile(
    module: Module
  )(implicit ctx: RuntimeContext): Either[Throwable, Module] = {
    val prevStage = module.getCompilationStage
    val compilationResult = Either.catchNonFatal {
      module.compileScope(ctx.executionService.getContext).getModule
    }
    if (prevStage != module.getCompilationStage) {
      ctx.executionService.getLogger.finest(
        s"Compiled ${module.getName} $prevStage->${module.getCompilationStage}"
      )
    }
    compilationResult
  }

  /** Apply pending edits to the file.
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

  /** Create cache invalidation commands after applying the edits.
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

  /** Run the invalidation commands.
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

  /** Send notification about module updates.
    *
    * @param payload the module update
    * @param ctx the runtime context
    */
  private def sendModuleUpdate(
    payload: Api.SuggestionsDatabaseModuleUpdateNotification
  )(implicit ctx: RuntimeContext): Unit =
    if (payload.updates.nonEmpty) {
      ctx.endpoint.sendToClient(Api.Response(payload))
    }

  /** Send notification about the compilation status.
    *
    * @param diagnostics the list of diagnostic messages returned by the
    * compiler
    * @param ctx the runtime context
    */
  private def sendDiagnosticUpdates(
    diagnostics: Seq[Api.ExecutionResult.Diagnostic]
  )(implicit ctx: RuntimeContext): Unit =
    if (diagnostics.nonEmpty) {
      ctx.contextManager.getAll.keys.foreach { contextId =>
        ctx.endpoint.sendToClient(
          Api.Response(Api.ExecutionUpdate(contextId, diagnostics))
        )
      }
    }

  /** Send notification about the compilation status.
    *
    * @param failure the execution failure
    * @param ctx the runtime context
    */
  private def sendFailureUpdate(
    failure: Api.ExecutionResult.Failure
  )(implicit ctx: RuntimeContext): Unit =
    ctx.contextManager.getAll.keys.foreach { contextId =>
      ctx.endpoint.sendToClient(
        Api.Response(Api.ExecutionFailed(contextId, failure))
      )
    }

  private def isSuggestionGlobal(suggestion: Suggestion): Boolean =
    suggestion match {
      case _: Suggestion.Atom     => true
      case _: Suggestion.Method   => true
      case _: Suggestion.Function => false
      case _: Suggestion.Local    => false
    }

  private def getCompilationStatus(
    diagnostics: Iterable[Api.ExecutionResult.Diagnostic]
  ): CompilationStatus =
    if (diagnostics.exists(_.kind == Api.DiagnosticType.Error()))
      CompilationStatus.Error
    else
      CompilationStatus.Success
}

object EnsureCompiledJob {

  /** The outcome of a compilation. */
  sealed trait CompilationStatus
  case object CompilationStatus {

    /** Compilation completed. */
    case object Success extends CompilationStatus

    /** Compilation completed with errors. */
    case object Error extends CompilationStatus

    /** Compiler crashed. */
    case object Failure extends CompilationStatus

    implicit val ordering: Ordering[CompilationStatus] =
      Ordering.by {
        case Success => 0
        case Error   => 1
        case Failure => 2
      }
  }

  private val unappliedEdits =
    new TrieMap[File, Seq[TextEdit]]()

  private def dequeueEdits(file: File): Seq[TextEdit] =
    unappliedEdits.remove(file).getOrElse(Seq())

  private def enqueueEdits(file: File, edits: Seq[TextEdit]): Unit =
    unappliedEdits.updateWith(file) {
      case Some(v) => Some(v :++ edits)
      case None    => Some(edits)
    }

  /** Create a job ensuring that files are compiled.
    *
    * @param files the list of files to compile
    * @return a new job
    */
  def apply(files: Iterable[File]): EnsureCompiledJob = {
    new EnsureCompiledJob(files)
  }

  /** Create a job ensuring that files are compiled after applying the edits.
    *
    * @param file a file to compile
    * @param edits the list of edits to apply
    * @return a new job
    */
  def apply(file: File, edits: Seq[TextEdit]): EnsureCompiledJob = {
    EnsureCompiledJob.enqueueEdits(file, edits)
    EnsureCompiledJob(List(file))
  }

  /** Create a job ensuring that modules are compiled.
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

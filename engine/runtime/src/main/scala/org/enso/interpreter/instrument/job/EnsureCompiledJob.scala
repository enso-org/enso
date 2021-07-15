package org.enso.interpreter.instrument.job

import java.io.{File, IOException}
import java.util.logging.Level

import cats.implicits._
import org.enso.compiler.context.{
  Changeset,
  ModuleContext,
  SuggestionBuilder,
  SuggestionDiff
}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.{
  CachePreferenceAnalysis,
  GatherDiagnostics
}
import org.enso.interpreter.instrument.{CacheInvalidation, InstrumentFrame}
import org.enso.interpreter.instrument.execution.{
  LocationResolver,
  RuntimeContext
}
import org.enso.interpreter.runtime.Module
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.buffer.Rope

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
      val compilationResult = ensureCompiledFiles(files)
      ctx.contextManager.getAll.values.foreach { stack =>
        getCacheMetadata(stack).foreach { metadata =>
          CacheInvalidation.run(
            stack,
            CacheInvalidation(
              CacheInvalidation.StackSelector.Top,
              CacheInvalidation.Command.SetMetadata(metadata)
            )
          )
        }
      }
      compilationResult
    } finally {
      ctx.locking.releaseWriteCompilationLock()
    }
  }

  /** Run the scheduled compilation and invalidation logic, and send the
    * suggestion updates.
    *
    * @param files the list of files to compile.
    * @param ctx the runtime context
    */
  protected def ensureCompiledFiles(
    files: Iterable[File]
  )(implicit ctx: RuntimeContext): CompilationStatus = {
    val modules = files.flatMap { file =>
      ctx.executionService.getContext.getModuleForFile(file).toScala
    }
    val moduleCompilationStatus = modules.map(ensureCompiledModule)
    val modulesInScope =
      getModulesInScope.filterNot(m => modules.exists(_ == m))
    val scopeCompilationStatus = ensureCompiledScope(modulesInScope)
    (moduleCompilationStatus ++ scopeCompilationStatus).maxOption
      .getOrElse(CompilationStatus.Success)
  }

  /** Run the scheduled compilation and invalidation logic, and send the
    * suggestion updates.
    *
    * @param module the module to compile.
    * @param ctx the runtime context
    */
  private def ensureCompiledModule(
    module: Module
  )(implicit ctx: RuntimeContext): CompilationStatus = {
    compile(module)
    val changeset = applyEdits(new File(module.getPath))
    compile(module)
      .map { module =>
        val cacheInvalidationCommands =
          buildCacheInvalidationCommands(changeset, module.getLiteralSource)
        runInvalidationCommands(cacheInvalidationCommands)
        if (ctx.executionService.getContext.isProjectSuggestionsEnabled) {
          analyzeModule(module, changeset)
        }
        runCompilationDiagnostics(module)
      }
      .getOrElse(CompilationStatus.Failure)
  }

  /** Compile all modules in the scope and send the extracted suggestions.
    *
    * @param ctx the runtime context
    */
  private def ensureCompiledScope(modulesInScope: Iterable[Module])(implicit
    ctx: RuntimeContext
  ): Iterable[CompilationStatus] = {
    ctx.executionService.getLogger
      .log(Level.FINEST, s"Modules in scope: ${modulesInScope.map(_.getName)}")
    modulesInScope
      .map { module =>
        compile(module) match {
          case Left(err) =>
            ctx.executionService.getLogger
              .log(Level.SEVERE, s"Compilation error in ${module.getName}", err)
            sendFailureUpdate(
              Api.ExecutionResult.Failure(
                err.getMessage,
                Option(module.getPath).map(new File(_))
              )
            )
            CompilationStatus.Failure
          case Right(module) =>
            if (ctx.executionService.getContext.isGlobalSuggestionsEnabled) {
              analyzeModuleInScope(module)
            }
            runCompilationDiagnostics(module)
        }
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
    if (!module.isIndexed && module.getLiteralSource != null) {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing module in scope ${module.getName}")
      val moduleName = module.getName
      val newSuggestions = SuggestionBuilder(module.getLiteralSource)
        .build(moduleName, module.getIr)
        .filter(isSuggestionGlobal)
      val version = ctx.versioning.evalVersion(module.getLiteralSource.toString)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        file    = getIndexingPath(module),
        version = version,
        actions =
          Vector(Api.SuggestionsDatabaseAction.Clean(moduleName.toString)),
        updates = SuggestionDiff.compute(Tree.empty, newSuggestions)
      )
      sendModuleUpdate(notification)
      module.setIndexed(true)
    }
  }

  private def analyzeModule(
    module: Module,
    changeset: Changeset[Rope]
  )(implicit ctx: RuntimeContext): Unit = {
    val moduleName = module.getName
    val version    = ctx.versioning.evalVersion(module.getLiteralSource.toString)
    if (module.isIndexed) {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing indexed module $moduleName")
      val prevSuggestions = SuggestionBuilder(changeset.source)
        .build(moduleName, changeset.ir)
      val newSuggestions =
        SuggestionBuilder(module.getLiteralSource)
          .build(moduleName, module.getIr)
      val diff = SuggestionDiff
        .compute(prevSuggestions, newSuggestions)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        file    = new File(module.getPath),
        version = version,
        actions = Vector(),
        updates = diff
      )
      sendModuleUpdate(notification)
    } else {
      ctx.executionService.getLogger
        .log(Level.FINEST, s"Analyzing not-indexed module ${module.getName}")
      val newSuggestions =
        SuggestionBuilder(module.getLiteralSource)
          .build(moduleName, module.getIr)
      val notification = Api.SuggestionsDatabaseModuleUpdateNotification(
        file    = new File(module.getPath),
        version = version,
        actions =
          Vector(Api.SuggestionsDatabaseAction.Clean(moduleName.toString)),
        updates = SuggestionDiff.compute(Tree.empty, newSuggestions)
      )
      sendModuleUpdate(notification)
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
    val pass = GatherDiagnostics
      .runModule(
        module.getIr,
        ModuleContext(
          module,
          compilerConfig = ctx.executionService.getContext.getCompilerConfig
        )
      )
      .unsafeGetMetadata(
        GatherDiagnostics,
        "No diagnostics metadata right after the gathering pass."
      )
      .diagnostics
    val diagnostics = pass.collect {
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
    Api.ExecutionResult.Diagnostic(
      kind,
      diagnostic.message,
      Option(module.getPath).map(new File(_)),
      diagnostic.location
        .map(loc =>
          LocationResolver
            .locationToRange(loc.location, module.getLiteralSource)
        ),
      diagnostic.location
        .flatMap(LocationResolver.getExpressionId(module.getIr, _))
        .map(_.externalId),
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
    ctx.executionService.getLogger
      .log(
        Level.FINEST,
        s"Compiled ${module.getName} $prevStage->${module.getCompilationStage}"
      )
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
      val edits = ctx.state.pendingEdits.dequeue(file)
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
      .foreach { stack =>
        if (stack.nonEmpty) {
          CacheInvalidation.runAll(stack, invalidationCommands)
        }
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
    if (payload.actions.nonEmpty || !payload.updates.isEmpty) {
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

  private def getCacheMetadata(
    stack: Iterable[InstrumentFrame]
  )(implicit ctx: RuntimeContext): Option[CachePreferenceAnalysis.Metadata] =
    stack.lastOption.flatMap {
      case InstrumentFrame(Api.StackItem.ExplicitCall(ptr, _, _), _, _) =>
        ctx.executionService.getContext.findModule(ptr.module).toScala.map {
          module =>
            module.getIr
              .unsafeGetMetadata(
                CachePreferenceAnalysis,
                s"Empty cache preference metadata ${module.getName}"
              )
        }
      case _ => None
    }

  /** Get all modules in the current compiler scope. */
  private def getModulesInScope(implicit
    ctx: RuntimeContext
  ): Iterable[Module] =
    ctx.executionService.getContext.getTopScope.getModules.asScala

  /** Get the module path for suggestions database indexing.
    *
    * If the module is synthetic (i.e. Builtins), uses its name as a path.
    */
  private def getIndexingPath(module: Module): File =
    if (module.getPath eq null) {
      new File(s"/${module.getName}")
    } else
      new File(module.getPath)

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

  /** Create [[EnsureCompiledJob]] for a single file.
    *
    * @param file the file to compile
    * @return new instance of [[EnsureCompiledJob]]
    */
  def apply(file: File): EnsureCompiledJob =
    new EnsureCompiledJob(Seq(file))

  /** Create [[EnsureCompiledJob]] for a stack.
    *
    * @param stack the call stack to compile
    * @return new instance of [[EnsureCompiledJob]]
    */
  def apply(stack: Iterable[InstrumentFrame])(implicit
    ctx: RuntimeContext
  ): EnsureCompiledJob =
    new EnsureCompiledJob(extractFiles(stack))

  /** Extract files to compile from a call stack.
    *
    * @param stack a call stack
    * @return a list of files to compile
    */
  private def extractFiles(stack: Iterable[InstrumentFrame])(implicit
    ctx: RuntimeContext
  ): Iterable[File] =
    stack
      .map(_.item)
      .flatMap {
        case Api.StackItem.ExplicitCall(methodPointer, _, _) =>
          ctx.executionService.getContext
            .findModule(methodPointer.module)
            .flatMap { module =>
              val path = java.util.Optional.ofNullable(module.getPath)
              if (path.isEmpty) {
                ctx.executionService.getLogger
                  .severe(s"${module.getName} module path is empty")
              }
              path
            }
            .map(path => new File(path))
            .toScala
        case _ =>
          None
      }

}

package org.enso.interpreter.instrument.job

import cats.implicits._
import org.enso.compiler.CompilerResult
import org.enso.compiler.context._
import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.{
  CachePreferenceAnalysis,
  GatherDiagnostics
}
import org.enso.interpreter.instrument.execution.{
  LocationResolver,
  RuntimeContext
}
import org.enso.interpreter.instrument.{
  CacheInvalidation,
  InstrumentFrame,
  Visualisation
}
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.service.error.ModuleNotFoundForFileException
import org.enso.pkg.QualifiedName
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.runtime.Runtime.Api.StackItem
import org.enso.text.buffer.Rope

import java.io.File
import java.util.logging.Level

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

/** A job that ensures that specified files are compiled.
  *
  * @param files a files to compile
  */
final class EnsureCompiledJob(protected val files: Iterable[File])
    extends Job[EnsureCompiledJob.CompilationStatus](List.empty, true, false) {

  import EnsureCompiledJob.CompilationStatus

  /** @inheritdoc */
  override def run(implicit ctx: RuntimeContext): CompilationStatus = {
    ctx.locking.acquireWriteCompilationLock()

    try {
      val compilationResult = ensureCompiledFiles(files)
      setCacheWeights(new java.util.HashSet[java.util.UUID]())
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
    (moduleCompilationStatus.flatten ++ scopeCompilationStatus).maxOption
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
  )(implicit ctx: RuntimeContext): Option[CompilationStatus] = {
    compile(module)
    applyEdits(new File(module.getPath)).map { changeset =>
      compile(module)
        .map { compilerResult =>
          invalidateCaches(module, changeset)
          ctx.jobProcessor.runBackground(
            AnalyzeModuleInScopeJob(
              module.getName,
              compilerResult.compiledModules
            )
          )
          ctx.jobProcessor.runBackground(AnalyzeModuleJob(module, changeset))
          runCompilationDiagnostics(module)
        }
        .getOrElse(CompilationStatus.Failure)
    }
  }

  /** Compile all modules in the scope and send the extracted suggestions.
    *
    * @param ctx the runtime context
    */
  private def ensureCompiledScope(modulesInScope: Iterable[Module])(implicit
    ctx: RuntimeContext
  ): Iterable[CompilationStatus] = {
    val notIndexedModulesInScope = modulesInScope.filter(!_.isIndexed)
    val (modulesToAnalyze, compilationStatuses) =
      notIndexedModulesInScope.foldLeft(
        (Set.newBuilder[Module], Vector.newBuilder[CompilationStatus])
      ) { case ((modules, statuses), module) =>
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
            (modules, statuses += CompilationStatus.Failure)
          case Right(compilerResult) =>
            val status = runCompilationDiagnostics(module)
            (
              modules.addAll(compilerResult.compiledModules).addOne(module),
              statuses += status
            )
        }
      }
    ctx.jobProcessor.runBackground(
      AnalyzeModuleInScopeJob(modulesToAnalyze.result())
    )
    compilationStatuses.result()
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
            .locationToRange(loc.location, module.getSource.getCharacters)
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
  )(implicit ctx: RuntimeContext): Either[Throwable, CompilerResult] =
    Either.catchNonFatal {
      val compilationStage = module.getCompilationStage
      if (!compilationStage.isAtLeast(Module.CompilationStage.AFTER_CODEGEN)) {
        ctx.executionService.getLogger
          .log(Level.FINEST, s"Compiling ${module.getName}.")
        val result = ctx.executionService.getContext.getCompiler.run(module)
        result.copy(compiledModules =
          result.compiledModules.filter(_.getName != module.getName)
        )
      } else {
        CompilerResult.empty
      }
    }

  /** Apply pending edits to the file.
    *
    * @param file the file to apply edits to
    * @param ctx the runtime context
    * @return the [[Changeset]] after applying the edits to the source
    */
  private def applyEdits(
    file: File
  )(implicit ctx: RuntimeContext): Option[Changeset[Rope]] = {
    ctx.locking.acquireFileLock(file)
    ctx.locking.acquireReadCompilationLock()
    ctx.locking.acquirePendingEditsLock()
    try {
      val pendingEdits = ctx.state.pendingEdits.dequeue(file)
      val edits        = pendingEdits.map(_.edit)
      val shouldExecute =
        pendingEdits.isEmpty || pendingEdits.exists(_.execute)
      val module = ctx.executionService.getContext
        .getModuleForFile(file)
        .orElseThrow(() => new ModuleNotFoundForFileException(file))
      val changesetBuilder = new ChangesetBuilder(
        module.getLiteralSource,
        module.getIr
      )
      val changeset = changesetBuilder.build(pendingEdits)
      ctx.executionService.modifyModuleSources(
        module,
        edits,
        changeset.simpleUpdate.orNull
      )
      Option.when(shouldExecute)(changeset)
    } finally {
      ctx.locking.releasePendingEditsLock()
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
    changeset: Changeset[_],
    source: CharSequence
  )(implicit ctx: RuntimeContext): Seq[CacheInvalidation] = {
    val invalidateExpressionsCommand =
      CacheInvalidation.Command.InvalidateKeys(changeset.invalidated)
    val scopeIds = ctx.executionService.getContext.getCompiler
      .parseMeta(source)
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
    * @param module the compiled module
    * @param changeset the changeset containing the list of invalidated expressions
    * @param ctx the runtime context
    */
  private def invalidateCaches(
    module: Module,
    changeset: Changeset[_]
  )(implicit ctx: RuntimeContext): Unit = {
    val invalidationCommands =
      buildCacheInvalidationCommands(
        changeset,
        module.getSource.getCharacters
      )
    val invalidatedKeys = new java.util.HashSet[java.util.UUID]();
    ctx.contextManager.getAllContexts.values
      .foreach { stack =>
        if (stack.nonEmpty && isStackInModule(module.getName, stack)) {
          CacheInvalidation.runAll(stack, invalidationCommands, invalidatedKeys)
        }
      }
    CacheInvalidation.runAllVisualisations(
      ctx.contextManager.getVisualisations(module.getName),
      invalidationCommands
    )

    val invalidatedKeysScala = invalidatedKeys.asScala.toSet
    ctx.contextManager.getAllContexts.keys
      .foreach { contextId =>
        ctx.endpoint.sendToClient(
          Api.Response(
            Api.ExpressionUpdates(
              contextId,
              invalidatedKeysScala.map { key =>
                Api.ExpressionUpdate(
                  key,
                  None,
                  None,
                  Vector.empty,
                  true,
                  Api.ExpressionUpdate.Payload.Panic("pending...", Seq(key))
                )
              }
            )
          )
        )
      }

    val invalidatedVisualisations =
      ctx.contextManager.getInvalidatedVisualisations(
        module.getName,
        changeset.invalidated
      )
    invalidatedVisualisations.foreach { visualisation =>
      UpsertVisualisationJob.upsertVisualisation(visualisation)
    }
    if (invalidatedVisualisations.nonEmpty) {
      ctx.executionService.getLogger.log(
        Level.FINE,
        s"Invalidated visualisations [${invalidatedVisualisations.map(_.id)}]"
      )
    }
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
      ctx.contextManager.getAllContexts.keys.foreach { contextId =>
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
    ctx.contextManager.getAllContexts.keys.foreach { contextId =>
      ctx.endpoint.sendToClient(
        Api.Response(Api.ExecutionFailed(contextId, failure))
      )
    }

  private def getCompilationStatus(
    diagnostics: Iterable[Api.ExecutionResult.Diagnostic]
  ): CompilationStatus =
    if (diagnostics.exists(_.kind == Api.DiagnosticType.Error()))
      CompilationStatus.Error
    else
      CompilationStatus.Success

  private def setCacheWeights(
    invalidatedKeys: java.util.Set[java.util.UUID]
  )(implicit ctx: RuntimeContext): Unit = {
    ctx.contextManager.getAllContexts.values.foreach { stack =>
      getCacheMetadata(stack).foreach { metadata =>
        CacheInvalidation.run(
          stack,
          CacheInvalidation(
            CacheInvalidation.StackSelector.Top,
            CacheInvalidation.Command.SetMetadata(metadata)
          ),
          invalidatedKeys
        )
      }
    }
    val visualisations = ctx.contextManager.getAllVisualisations
    visualisations.flatMap(getCacheMetadata).foreach { metadata =>
      CacheInvalidation.runVisualisations(
        visualisations,
        CacheInvalidation.Command.SetMetadata(metadata)
      )
    }
  }

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

  private def getCacheMetadata(
    visualisation: Visualisation
  ): Option[CachePreferenceAnalysis.Metadata] = {
    val module = visualisation.module
    module.getIr.getMetadata(CachePreferenceAnalysis)
  }

  /** Get all modules in the current compiler scope. */
  private def getModulesInScope(implicit
    ctx: RuntimeContext
  ): Iterable[Module] =
    ctx.executionService.getContext.getTopScope.getModules.asScala

  /** Check if stack belongs to the provided module.
    *
    * @param module the qualified module name
    * @param stack the execution stack
    */
  private def isStackInModule(
    module: QualifiedName,
    stack: Iterable[InstrumentFrame]
  ): Boolean =
    stack.headOption match {
      case Some(
            InstrumentFrame(StackItem.ExplicitCall(methodPointer, _, _), _, _)
          ) =>
        methodPointer.module == module.toString
      case _ =>
        false
    }

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

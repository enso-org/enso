package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import com.oracle.truffle.api.source.Source
import org.enso.compiler.codegen.{IrToTruffle, RuntimeStubsGenerator}
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.exception.{CompilationAbortedException, CompilerError}
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse._
import org.enso.compiler.phase.{
  ExportCycleException,
  ExportsResolution,
  ImportResolver
}
import org.enso.editions.LibraryName
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}
import org.enso.interpreter.runtime.{EnsoContext, Module}
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.enso.syntax.text.Parser.IDMap
import org.enso.syntax.text.Parser
import org.enso.syntax2.Tree

import java.io.{PrintStream, StringReader}
import java.util.concurrent.{
  CompletableFuture,
  ExecutorService,
  LinkedBlockingDeque,
  ThreadPoolExecutor,
  TimeUnit
}
import java.util.logging.Level

import scala.jdk.OptionConverters._

/** This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  *
  * @param context the language context
  */
class Compiler(
  val context: EnsoContext,
  val builtins: Builtins,
  val packageRepository: PackageRepository,
  config: CompilerConfig
) {
  private val freshNameSupply: FreshNameSupply = new FreshNameSupply
  private val passes: Passes                   = new Passes(config)
  private val passManager: PassManager         = passes.passManager
  private val importResolver: ImportResolver   = new ImportResolver(this)
  private val stubsGenerator: RuntimeStubsGenerator =
    new RuntimeStubsGenerator(builtins)
  private val irCachingEnabled = !context.isIrCachingDisabled
  private val useGlobalCacheLocations = context.getEnvironment.getOptions.get(
    RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION_KEY
  )
  private val isInteractiveMode =
    context.getEnvironment.getOptions.get(RuntimeOptions.INTERACTIVE_MODE_KEY)
  private val serializationManager: SerializationManager =
    new SerializationManager(this)
  private val logger: TruffleLogger = context.getLogger(getClass)
  private val output: PrintStream =
    if (config.outputRedirect.isDefined)
      new PrintStream(config.outputRedirect.get)
    else context.getOut
  private lazy val ensoCompiler: EnsoCompiler = new EnsoCompiler()

  /** The thread pool that handles parsing of modules. */
  private val pool: ExecutorService = if (config.parallelParsing) {
    new ThreadPoolExecutor(
      Compiler.startingThreadCount,
      Compiler.maximumThreadCount,
      Compiler.threadKeepalive,
      TimeUnit.SECONDS,
      new LinkedBlockingDeque[Runnable](),
      (runnable: Runnable) => {
        context.getEnvironment.createThread(runnable)
      }
    )
  } else null

  /** Duplicates this compiler with a different config.
    * @param newConfig Configuration to be used in the duplicated Compiler.
    */
  def duplicateWithConfig(newConfig: CompilerConfig): Compiler = {
    new Compiler(
      context,
      builtins,
      packageRepository,
      newConfig
    )
  }

  /** Run the initialization sequence. */
  def initialize(): Unit = {
    initializeBuiltinsIr()
    packageRepository.initialize().left.foreach(reportPackageError)
  }

  /** Lazy-initializes the IR for the builtins module. */
  private def initializeBuiltinsIr(): Unit = {
    if (!builtins.isIrInitialized) {
      logger.log(
        Compiler.defaultLogLevel,
        "Initialising IR for [{}].",
        builtins.getModule.getName
      )

      builtins.initializeBuiltinsSource()

      if (irCachingEnabled) {
        serializationManager.deserialize(builtins.getModule) match {
          case Some(true) =>
            // Ensure that builtins doesn't try and have codegen run on it.
            builtins.getModule.unsafeSetCompilationStage(
              Module.CompilationStage.AFTER_CODEGEN
            )
          case _ =>
            builtins.initializeBuiltinsIr(freshNameSupply, passes)
            builtins.getModule.setHasCrossModuleLinks(true)

        }
      } else {
        builtins.initializeBuiltinsIr(freshNameSupply, passes)
        builtins.getModule.setHasCrossModuleLinks(true)
      }

      if (irCachingEnabled && !builtins.getModule.wasLoadedFromCache()) {
        serializationManager.serializeModule(
          builtins.getModule,
          useGlobalCacheLocations = true // Builtins can't have a local cache.
        )
      }
    }
  }

  /** @return the serialization manager instance. */
  def getSerializationManager: SerializationManager =
    serializationManager

  /** Processes the provided language sources, registering any bindings in the
    * given scope.
    *
    * @param module the scope into which new bindings are registered
    * @return a compiler result containing the list of compiled modules
    */
  def run(module: Module): CompilerResult = {
    runInternal(
      List(module),
      generateCode              = true,
      shouldCompileDependencies = true
    )
  }

  /** Compiles the requested packages, writing the compiled IR to the library
    * cache directories.
    *
    * @param shouldCompileDependencies whether compilation should also compile
    *                                  the dependencies of the requested package
    * @param useGlobalCacheLocations whether or not the compilation result should
    *                                  be written to the global cache
    */
  def compile(
    shouldCompileDependencies: Boolean,
    useGlobalCacheLocations: Boolean
  ): Unit = {
    val packageRepository = context.getPackageRepository

    packageRepository.getMainProjectPackage match {
      case None =>
        logger.log(
          Level.SEVERE,
          "No package found in the compiler environment. Aborting."
        )
      case Some(pkg) =>
        val packageModule = packageRepository.getModuleMap.get(
          s"${pkg.namespace}.${pkg.name}.Main"
        )
        packageModule match {
          case None =>
            logger.log(
              Level.SEVERE,
              "Could not find entry point for compilation in package [{}]",
              s"${pkg.namespace}.${pkg.name}"
            )
          case Some(m) =>
            logger.log(
              Compiler.defaultLogLevel,
              s"Compiling the package [${pkg.namespace}.${pkg.name}] " +
              s"starting at the root [${m.getName}]."
            )

            val packageModules = packageRepository.freezeModuleMap.collect {
              case (name, mod)
                  if name.startsWith(s"${pkg.namespace}.${pkg.name}") =>
                mod
            }.toList

            runInternal(
              packageModules,
              generateCode = false,
              shouldCompileDependencies
            )

            serializationManager.serializeLibrary(
              pkg.libraryName,
              useGlobalCacheLocations = useGlobalCacheLocations
            )
        }
    }
  }

  /** Runs part of the compiler to generate docs from Enso code.
    *
    * @param module the scope from which docs are generated
    */
  def generateDocs(module: Module): Module = {
    initialize()
    parseModule(module, isGenDocs = true)
    module
  }

  /** Run the compiler on the list of modules.
    *
    * The compilation may load the libraries defining component groups. To ensure
    * that the symbols defined by the component groups are also compiled, this
    * method is called recursively.
    */
  private def runInternal(
    modules: List[Module],
    generateCode: Boolean,
    shouldCompileDependencies: Boolean
  ): CompilerResult = {
    @scala.annotation.tailrec
    def go(
      modulesToCompile: List[Module],
      compiledModules: List[Module]
    ): CompilerResult =
      if (modulesToCompile.isEmpty) CompilerResult(compiledModules)
      else {
        val newCompiled =
          runCompilerPipeline(
            modulesToCompile,
            generateCode,
            shouldCompileDependencies
          )
        val pending = packageRepository.getPendingModules.toList
        go(pending, compiledModules ++ newCompiled)
      }

    go(modules, List())
  }

  private def runCompilerPipeline(
    modules: List[Module],
    generateCode: Boolean,
    shouldCompileDependencies: Boolean
  ): List[Module] = {
    initialize()
    modules.foreach(m => parseModule(m))

    var requiredModules = modules.flatMap { module =>
      val modules = runImportsAndExportsResolution(module, generateCode)
      if (
        module
          .wasLoadedFromCache() && modules
          .exists(m => !m.wasLoadedFromCache() && !m.isSynthetic)
      ) {
        logger.log(
          Compiler.defaultLogLevel,
          "Some imported modules' caches were invalided, forcing invalidation of {}",
          module.getName.toString
        )
        module.getCache.invalidate(context)
        parseModule(module)
        runImportsAndExportsResolution(module, generateCode)
      } else {
        modules
      }
    }.distinct

    var hasInvalidModuleRelink = false
    if (irCachingEnabled) {
      requiredModules.foreach { module =>
        if (!module.hasCrossModuleLinks) {
          val flags =
            module.getIr.preorder.map(_.passData.restoreFromSerialization(this))

          if (!flags.contains(false)) {
            logger.log(
              Compiler.defaultLogLevel,
              "Restored links (late phase) for module [{}].",
              module.getName
            )
          } else {
            hasInvalidModuleRelink = true
            logger.log(
              Compiler.defaultLogLevel,
              "Failed to restore links (late phase) for module [{}].",
              module.getName
            )
            uncachedParseModule(module, isGenDocs = false)
          }
        }
      }
    }

    if (hasInvalidModuleRelink) {
      logger.log(
        Compiler.defaultLogLevel,
        s"Some modules failed to relink. Re-running import and " +
        s"export resolution."
      )

      requiredModules =
        modules.flatMap(runImportsAndExportsResolution(_, generateCode))
    }

    requiredModules.foreach { module =>
      if (
        !module.getCompilationStage.isAtLeast(
          Module.CompilationStage.AFTER_GLOBAL_TYPES
        )
      ) {

        val moduleContext = ModuleContext(
          module          = module,
          freshNameSupply = Some(freshNameSupply),
          compilerConfig  = config
        )
        val compilerOutput = runGlobalTypingPasses(module.getIr, moduleContext)
        module.unsafeSetIr(compilerOutput)
        module.unsafeSetCompilationStage(
          Module.CompilationStage.AFTER_GLOBAL_TYPES
        )
      }
    }
    requiredModules.foreach { module =>
      if (
        !module.getCompilationStage.isAtLeast(
          Module.CompilationStage.AFTER_STATIC_PASSES
        )
      ) {

        val moduleContext = ModuleContext(
          module          = module,
          freshNameSupply = Some(freshNameSupply),
          compilerConfig  = config,
          pkgRepo         = Some(packageRepository)
        )
        val compilerOutput = runMethodBodyPasses(module.getIr, moduleContext)
        module.unsafeSetIr(compilerOutput)
        module.unsafeSetCompilationStage(
          Module.CompilationStage.AFTER_STATIC_PASSES
        )
      }
    }

    runErrorHandling(requiredModules)

    requiredModules.foreach { module =>
      if (
        !module.getCompilationStage.isAtLeast(
          Module.CompilationStage.AFTER_RUNTIME_STUBS
        )
      ) {
        stubsGenerator.run(module)
        module.unsafeSetCompilationStage(
          Module.CompilationStage.AFTER_RUNTIME_STUBS
        )
      }
    }
    requiredModules.foreach { module =>
      if (
        !module.getCompilationStage.isAtLeast(
          Module.CompilationStage.AFTER_CODEGEN
        )
      ) {

        if (generateCode) {
          logger.log(
            Compiler.defaultLogLevel,
            "Generating code for module [{}].",
            module.getName
          )

          truffleCodegen(module.getIr, module.getSource, module.getScope)
        }
        module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_CODEGEN)

        if (shouldCompileDependencies || isModuleInRootPackage(module)) {
          val shouldStoreCache =
            irCachingEnabled && !module.wasLoadedFromCache()
          if (shouldStoreCache && !hasErrors(module) && !module.isInteractive) {
            if (isInteractiveMode) {
              context.getNotificationHandler.serializeModule(module.getName)
            } else {
              serializationManager.serializeModule(
                module,
                useGlobalCacheLocations
              )
            }
          }
        } else {
          logger.log(
            Compiler.defaultLogLevel,
            "Skipping serialization for [{}].",
            module.getName
          )
        }
      }
    }

    requiredModules
  }

  private def isModuleInRootPackage(module: Module): Boolean = {
    if (!module.isInteractive) {
      val pkg = context.getPackageOf(module.getSourceFile).toScala
      pkg.contains(context.getPackageRepository.getMainProjectPackage.get)
    } else false
  }

  private def runImportsAndExportsResolution(
    module: Module,
    bindingsCachingEnabled: Boolean
  ): List[Module] = {
    val (importedModules, modulesImportedWithCachedBindings) =
      try {
        importResolver.mapImports(module, bindingsCachingEnabled)
      } catch {
        case e: ImportResolver.HiddenNamesConflict => reportExportConflicts(e)
      }

    val requiredModules =
      try { new ExportsResolution().run(importedModules) }
      catch { case e: ExportCycleException => reportCycle(e) }

    val parsingTasks: List[CompletableFuture[Unit]] =
      modulesImportedWithCachedBindings.map { module =>
        if (config.parallelParsing) {
          CompletableFuture.supplyAsync(
            () => ensureParsedAndAnalyzed(module),
            pool
          )
        } else {
          CompletableFuture.completedFuture(ensureParsedAndAnalyzed(module))
        }
      }

    joinAllFutures(parsingTasks).get()

    // ** Order matters for codegen **
    // Consider a case when an exported symbol is referenced but the module that defines the symbol
    // has not yet registered the method in its scope. This will result in No_Such_Method method during runtime;
    // the symbol brought to the scope has not been properly resolved yet.
    val sortedCachedModules =
      new ExportsResolution().runSort(modulesImportedWithCachedBindings)
    sortedCachedModules ++ requiredModules
  }

  private def ensureParsedAndAnalyzed(module: Module): Unit = {
    ensureParsed(module)
    if (module.isSynthetic) {
      // Synthetic modules need to be import-analyzed
      // i.e. we need to fill in resolved{Imports/Exports} and exportedSymbols in bindings
      // because we do not generate (and deserialize) IR for them
      // TODO: consider generating IR for synthetic modules, if possible.
      importExportBindings(module) match {
        case Some(bindings) =>
          val converted = bindings
            .toConcrete(packageRepository.getModuleMap)
            .map { concreteBindings =>
              concreteBindings
            }
          val ir = module.getIr
          val currentLocal = ir.unsafeGetMetadata(
            BindingAnalysis,
            "Synthetic parsed module missing bindings"
          )
          currentLocal.resolvedImports =
            converted.map(_.resolvedImports).getOrElse(Nil)
          currentLocal.resolvedExports =
            converted.map(_.resolvedExports).getOrElse(Nil)
          currentLocal.exportedSymbols =
            converted.map(_.exportedSymbols).getOrElse(Map.empty)
        case _ =>
      }
    }
  }

  private def joinAllFutures[T](
    futures: List[CompletableFuture[T]]
  ): CompletableFuture[List[T]] = {
    CompletableFuture.allOf(futures: _*).thenApply(_ => futures.map(_.join()))
  }

  /** Runs the initial passes of the compiler to gather the import statements,
    * used for dependency resolution.
    *
    * @param module - the scope from which docs are generated.
    */
  def gatherImportStatements(module: Module): Array[String] = {
    ensureParsed(module)
    val importedModules = module.getIr.imports.flatMap {
      case imp: IR.Module.Scope.Import.Module =>
        imp.name.parts.take(2).map(_.name) match {
          case List(namespace, name) => List(LibraryName(namespace, name))
          case _ =>
            throw new CompilerError(s"Invalid module name: [${imp.name}].")
        }

      case _: IR.Module.Scope.Import.Polyglot =>
        // Note [Polyglot Imports In Dependency Gathering]
        Nil
      case other =>
        throw new CompilerError(
          s"Unexpected import type after processing ${module.getName}: [$other]."
        )
    }
    importedModules.distinct.map(_.qualifiedName).toArray
  }

  private def parseModule(
    module: Module,
    isGenDocs: Boolean = false
  ): Unit = {
    logger.log(
      Compiler.defaultLogLevel,
      "Parsing module [{}].",
      module.getName
    )
    module.ensureScopeExists(context)
    module.getScope.reset()

    if (irCachingEnabled && !module.isInteractive) {
      serializationManager.deserialize(module) match {
        case Some(_) => return
        case _       =>
      }
    }

    uncachedParseModule(module, isGenDocs)
  }

  /** Retrieve module bindings from cache, if available.
    *
    * @param module module which is conssidered
    * @return module's bindings, if available in libraries' bindings cache
    */
  def importExportBindings(module: Module): Option[BindingsMap] = {
    if (irCachingEnabled && !module.isInteractive) {
      val libraryName = Option(module.getPackage).map(_.libraryName)
      libraryName
        .flatMap(packageRepository.getLibraryBindings(_, serializationManager))
        .flatMap(_.bindings.entries.get(module.getName))
    } else None
  }

  private def uncachedParseModule(module: Module, isGenDocs: Boolean): Unit = {
    logger.log(
      Compiler.defaultLogLevel,
      "Loading module [{}] from source.",
      module.getName
    )
    module.ensureScopeExists(context)
    module.getScope.reset()

    val moduleContext = ModuleContext(
      module           = module,
      freshNameSupply  = Some(freshNameSupply),
      compilerConfig   = config,
      isGeneratingDocs = isGenDocs
    )

    val src  = module.getSource
    val tree = ensoCompiler.parse(src)
    val expr = ensoCompiler.generateIR(tree)

    val exprWithModuleExports =
      if (module.isSynthetic)
        expr
      else
        injectSyntheticModuleExports(expr, module.getDirectModulesRefs)
    val discoveredModule =
      recognizeBindings(exprWithModuleExports, moduleContext)
    module.unsafeSetIr(discoveredModule)
    module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_PARSING)
    module.setLoadedFromCache(false)
    module.setHasCrossModuleLinks(true)
  }

  /* Note [Polyglot Imports In Dependency Gathering]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Currently we just ignore polyglot imports when gathering the dependencies -
   * we assume that the project itself or one of its dependencies will contain
   * in their `polyglot` directory any JARs that need to be included in the
   * classpath for this import to be resolved.
   *
   * In the future we may want to extend the edition system with some settings
   * for automatically resolving the Java dependencies using a system based on
   * Maven, but currently the libraries just must include their binary
   * dependencies.
   */

  /** Gets a module definition by name.
    *
    * @param name the name of module to look up
    * @return the module corresponding to the provided name, if exists
    */
  def getModule(name: String): Option[Module] = {
    context.getTopScope.getModule(name).toScala
  }

  /** Ensures the passed module is in at least the parsed compilation stage.
    *
    * @param module the module to ensure is parsed.
    */
  def ensureParsed(module: Module): Unit = {
    if (
      !module.getCompilationStage.isAtLeast(
        Module.CompilationStage.AFTER_PARSING
      )
    ) {
      parseModule(module)
    }
  }

  /** Processes the language source, interpreting it as an expression.
    * Processes the source in the context of given local and module scopes.
    *
    * @param srcString string representing the expression to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return an expression node representing the parsed and analyzed source
    */
  def runInline(
    srcString: String,
    inlineContext: InlineContext
  ): Option[RuntimeExpression] = {
    val newContext = inlineContext.copy(freshNameSupply = Some(freshNameSupply))
    val source = Source
      .newBuilder(
        LanguageInfo.ID,
        new StringReader(srcString),
        "<interactive_source>"
      )
      .build()
    val tree = ensoCompiler.parse(source)

    ensoCompiler.generateIRInline(tree).flatMap { ir =>
      val compilerOutput = runCompilerPhasesInline(ir, newContext)
      runErrorHandlingInline(compilerOutput, source, newContext)
      Some(truffleCodegenInline(compilerOutput, source, newContext))
    }
  }

  /** Finds and processes a language source by its qualified name.
    *
    * The results of this operation are cached internally so we do not need to
    * process the same source file multiple times.
    *
    * @param qualifiedName the qualified name of the module
    * @param loc the location of the import
    * @return the scope containing all definitions in the requested module
    */
  def processImport(
    qualifiedName: String,
    loc: Option[IR.IdentifiedLocation],
    source: Source
  ): ModuleScope = {
    val module = context.getTopScope
      .getModule(qualifiedName)
      .toScala
      .getOrElse {
        val locStr = fileLocationFromSection(loc, source)
        throw new CompilerError(
          s"Attempted to import the unresolved module $qualifiedName " +
          s"during code generation. Defined at $locStr."
        )
      }
    if (
      !module.getCompilationStage.isAtLeast(
        Module.CompilationStage.AFTER_RUNTIME_STUBS
      )
    ) {
      throw new CompilerError(
        "Trying to use a module in codegen without generating runtime stubs"
      )
    }
    module.getScope
  }

  /** Parses the given source with the new Rust parser.
    *
    * @param source The inline code to parse
    * @return A Tree representation of `source`
    */
  def parseInline(source: Source): Tree = ensoCompiler.parse(source)

  /** Parses the metadata of the provided language sources.
    *
    * @param source the code to parse
    * @return the source metadata
    */
  def parseMeta(source: CharSequence): IDMap =
    Parser().splitMeta(source.toString)._2

  /** Enhances the provided IR with import/export statements for the provided list
    * of fully qualified names of modules. The statements are considered to be "synthetic" i.e. compiler-generated.
    * That way one can access modules using fully qualified names.
    * E.g.,
    * Given module A/B/C.enso
    * ````
    *   type C
    *       C a
    * ````
    * it is possible to
    * ```
    * import A
    * ...
    *   x = A.B.C 0
    * ```
    * because the compiler will inject synthetic modules A and A.B such that
    * A.enso:
    * ````
    *   import project.A.B
    *   export project.A.B
    * ````
    * and A/B.enso:
    * ````
    *   import project.A.B.C
    *   export project.A.B.C
    * ````
    *
    * @param ir IR to be enhanced
    * @param modules fully qualified names of modules
    * @return enhanced
    */
  private def injectSyntheticModuleExports(
    ir: IR.Module,
    modules: java.util.List[QualifiedName]
  ): IR.Module = {
    import scala.jdk.CollectionConverters._

    val moduleNames = modules.asScala.map { q =>
      val name = q.path.foldRight(
        List(IR.Name.Literal(q.item, isMethod = false, location = None))
      ) { case (part, acc) =>
        IR.Name.Literal(part, isMethod = false, location = None) :: acc
      }
      IR.Name.Qualified(name, location = None)
    }.toList
    ir.copy(
      imports = ir.imports ::: moduleNames.map(m =>
        IR.Module.Scope.Import.Module(
          m,
          rename      = None,
          isAll       = false,
          onlyNames   = None,
          hiddenNames = None,
          location    = None,
          isSynthetic = true
        )
      ),
      exports = ir.exports ::: moduleNames.map(m =>
        IR.Module.Scope.Export.Module(
          m,
          rename      = None,
          isAll       = false,
          onlyNames   = None,
          hiddenNames = None,
          location    = None,
          isSynthetic = true
        )
      )
    )
  }

  private def recognizeBindings(
    module: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    passManager.runPassesOnModule(
      module,
      moduleContext,
      passes.moduleDiscoveryPasses
    )
  }

  /** Runs the various compiler passes.
    *
    * @param ir the compiler intermediate representation to transform
    * @return the output result of the
    */
  private def runMethodBodyPasses(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    passManager.runPassesOnModule(ir, moduleContext, passes.functionBodyPasses)
  }

  private def runGlobalTypingPasses(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    passManager.runPassesOnModule(ir, moduleContext, passes.globalTypingPasses)
  }

  /** Runs the various compiler passes in an inline context.
    *
    * @param ir the compiler intermediate representation to transform
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return the output result of the
    */
  def runCompilerPhasesInline(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    passManager.runPassesInline(ir, inlineContext)
  }

  /** Runs the strict error handling mechanism (if enabled in the language
    * context) for the inline compiler flow.
    *
    * @param ir the IR after compilation passes.
    * @param source the original source code.
    * @param inlineContext the inline compilation context.
    */
  def runErrorHandlingInline(
    ir: IR.Expression,
    source: Source,
    inlineContext: InlineContext
  ): Unit =
    if (config.isStrictErrors) {
      val errors = GatherDiagnostics
        .runExpression(ir, inlineContext)
        .unsafeGetMetadata(
          GatherDiagnostics,
          "No diagnostics metadata right after the gathering pass."
        )
        .diagnostics
      if (reportDiagnostics(errors, source)) {
        throw new CompilationAbortedException
      }
    }

  /** Runs the strict error handling mechanism (if enabled in the language
    * context) for the module-level compiler flow.
    *
    * @param modules the modules to check against errors
    */
  def runErrorHandling(
    modules: List[Module]
  ): Unit = {
    if (config.isStrictErrors) {
      val diagnostics = modules.flatMap { module =>
        val errors = gatherDiagnostics(module)
        List((module, errors))
      }
      if (reportDiagnostics(diagnostics)) {
        val count =
          diagnostics.map(_._2.collect { case e: IR.Error => e }.length).sum
        val warnCount =
          diagnostics.map(_._2.collect { case e: IR.Warning => e }.length).sum
        context.getErr.println(
          s"Aborting due to ${count} errors and ${warnCount} warnings."
        )
        throw new CompilationAbortedException
      }
    }
  }

  /** Gathers diagnostics for a single module.
    *
    * @param module the module for which to gather diagnostics
    * @return the diagnostics from the module
    */
  def gatherDiagnostics(module: Module): List[IR.Diagnostic] = {
    GatherDiagnostics
      .runModule(
        module.getIr,
        ModuleContext(module, compilerConfig = config)
      )
      .unsafeGetMetadata(
        GatherDiagnostics,
        "No diagnostics metadata right after the gathering pass."
      )
      .diagnostics
  }

  private def hasErrors(module: Module): Boolean =
    gatherDiagnostics(module).exists {
      case _: IR.Error => true
      case _           => false
    }

  private def reportCycle(exception: ExportCycleException): Nothing = {
    if (config.isStrictErrors) {
      output.println("Compiler encountered errors:")
      output.println("Export statements form a cycle:")
      exception.modules match {
        case List(mod) =>
          output.println(s"    ${mod.getName} exports itself.")
        case first :: second :: rest =>
          output.println(
            s"    ${first.getName} exports ${second.getName}"
          )
          rest.foreach { mod =>
            output.println(s"    which exports ${mod.getName}")
          }
          output.println(
            s"    which exports ${first.getName}, forming a cycle."
          )
        case _ =>
      }
      throw new CompilationAbortedException
    } else {
      throw exception
    }
  }

  private def reportExportConflicts(exception: Throwable): Nothing = {
    if (config.isStrictErrors) {
      output.println("Compiler encountered errors:")
      output.println(exception.getMessage)
      throw new CompilationAbortedException
    } else {
      throw exception
    }
  }

  /** Report the errors encountered when initializing the package repository.
    *
    * @param err the package repository error
    */
  private def reportPackageError(err: PackageRepository.Error): Unit = {
    output.println(
      s"In package description ${org.enso.pkg.Package.configFileName}:"
    )
    output.println("Compiler encountered warnings:")
    output.println(err.toString)
  }

  /** Reports diagnostics from multiple modules.
    *
    * @param diagnostics the mapping between modules and existing diagnostics.
    * @return whether any errors were encountered.
    */
  private def reportDiagnostics(
    diagnostics: List[(Module, List[IR.Diagnostic])]
  ): Boolean = {
    // It may be tempting to replace `.foldLeft(..)` with
    // `.find(...).nonEmpty. Don't. We want to report diagnostics for all modules
    // not just the first one.
    diagnostics
      .foldLeft(false) { case (result, (mod, diags)) =>
        if (diags.nonEmpty) {
          output.println(s"In module ${mod.getName}:")
          reportDiagnostics(diags, mod.getSource) || result
        } else {
          result
        }
      }
  }

  /** Reports compilation diagnostics to the standard output and throws an
    * exception breaking the execution flow if there are errors.
    *
    * @param diagnostics all the diagnostics found in the program IR.
    * @param source the original source code.
    * @return whether any errors were encountered.
    */
  private def reportDiagnostics(
    diagnostics: List[IR.Diagnostic],
    source: Source
  ): Boolean = {
    val errors   = diagnostics.collect { case e: IR.Error => e }
    val warnings = diagnostics.collect { case w: IR.Warning => w }

    if (warnings.nonEmpty) {
      output.println("Compiler encountered warnings:")
      warnings.foreach { warning =>
        output.println(formatDiagnostic(warning, source))
      }
    }

    if (errors.nonEmpty) {
      output.println("Compiler encountered errors:")
      errors.foreach { error =>
        output.println(formatDiagnostic(error, source))
      }
      true
    } else {
      false
    }
  }

  /** Pretty prints compiler diagnostics.
    *
    * @param diagnostic the diagnostic to pretty print
    * @param source the original source code
    * @return the result of pretty printing `diagnostic`
    */
  private def formatDiagnostic(
    diagnostic: IR.Diagnostic,
    source: Source
  ): String = {
    fileLocationFromSection(
      diagnostic.location,
      source
    ) + ": " + diagnostic.message
  }

  private def fileLocationFromSection(
    loc: Option[IR.IdentifiedLocation],
    source: Source
  ): String = {
    val srcLocation = loc
      .map { loc =>
        val section =
          source.createSection(loc.location.start, loc.location.length)
        val locStr =
          "" + section.getStartLine + ":" +
          section.getStartColumn + "-" +
          section.getEndLine + ":" +
          section.getEndColumn
        "[" + locStr + "]"
      }
      .getOrElse("")
    source.getName + srcLocation
  }

  /** Generates code for the truffle interpreter.
    *
    * @param ir the program to translate
    * @param source the source code of the program represented by `ir`
    * @param scope the module scope in which the code is to be generated
    */
  def truffleCodegen(
    ir: IR.Module,
    source: Source,
    scope: ModuleScope
  ): Unit = {
    new IrToTruffle(context, source, scope, config).run(ir)
  }

  /** Generates code for the truffle interpreter in an inline context.
    *
    * @param ir the prorgam to translate
    * @param source the source code of the program represented by `ir`
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return the runtime representation of the program represented by `ir`
    */
  def truffleCodegenInline(
    ir: IR.Expression,
    source: Source,
    inlineContext: InlineContext
  ): RuntimeExpression = {
    new IrToTruffle(
      context,
      source,
      inlineContext.module.getScope,
      config
    ).runInline(
      ir,
      inlineContext.localScope.getOrElse(LocalScope.root),
      "<inline_source>"
    )
  }

  /** Performs shutdown actions for the compiler.
    *
    * @param waitForPendingJobCompletion whether or not shutdown should wait for
    *                                    jobs to complete
    */
  def shutdown(waitForPendingJobCompletion: Boolean): Unit = {
    serializationManager.shutdown(waitForPendingJobCompletion)
    shutdownParsingPool(waitForPendingJobCompletion)
  }

  private def shutdownParsingPool(waitForPendingCompilation: Boolean): Unit = {
    if (pool != null) {
      if (waitForPendingCompilation) {
        pool.shutdown()

        // Bound the waiting loop
        val maxCount = 10
        var counter  = 0
        while (!pool.isTerminated && counter < maxCount) {
          counter += 1
          pool.awaitTermination((50 * counter).toLong, TimeUnit.MILLISECONDS)
        }

        pool.shutdownNow()
        Thread.sleep(100)
      } else {
        pool.shutdownNow()
      }
    }
  }

  /** Updates the metadata in a copy of the IR when updating that metadata
    * requires global state.
    *
    * This is usually the case in the presence of structures that are shared
    * throughout the IR, and need to maintain that sharing for correctness. This
    * must be called with `copyOfIr` as the result of an `ir.duplicate` call.
    *
    * Additionally this method _must not_ alter the structure of the IR. It
    * should only update its metadata.
    *
    * @param sourceIr the IR being copied
    * @param copyOfIr a duplicate of `sourceIr`
    * @return the result of updating metadata in `copyOfIr` globally using
    *         information from `sourceIr`
    */
  def updateMetadata(sourceIr: IR.Module, copyOfIr: IR.Module): IR.Module = {
    passManager.runMetadataUpdate(sourceIr, copyOfIr)
  }
}
object Compiler {

  /** The default logging level for the compiler. */
  private val defaultLogLevel: Level = Level.FINE

  /** The maximum number of parsing threads allowed. */
  val maximumThreadCount: Integer = 10

  /** The number of threads at compiler start. */
  val startingThreadCount: Integer = 2

  /** The thread keep-alive time in seconds. */
  val threadKeepalive: Long = 2
}

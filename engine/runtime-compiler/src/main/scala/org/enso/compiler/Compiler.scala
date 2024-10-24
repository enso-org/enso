package org.enso.compiler

import scala.jdk.CollectionConverters.IterableHasAsJava
import org.enso.compiler.context.{
  CompilerContext,
  FreshNameSupply,
  InlineContext,
  ModuleContext
}
import org.enso.compiler.context.CompilerContext.Module
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  Diagnostic,
  Expression,
  Name,
  Warning,
  Module => IRModule
}
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.module.scope.imports
import org.enso.compiler.core.EnsoParser
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse._
import org.enso.compiler.phase.{ImportResolver, ImportResolverAlgorithm}
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName
import org.enso.common.CompilationStage
import org.enso.compiler.phase.exports.{
  ExportCycleException,
  ExportSymbolAnalysis,
  ExportsResolution
}
import org.enso.syntax2.Tree
import org.enso.syntax2.Parser

import java.io.PrintStream
import java.util.concurrent.{
  CompletableFuture,
  ExecutorService,
  Future,
  LinkedBlockingDeque,
  ThreadPoolExecutor,
  TimeUnit
}
import java.util.logging.Level

/** This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  *
  * @param context the language context
  */
class Compiler(
  val context: CompilerContext,
  val packageRepository: PackageRepository,
  private val config: CompilerConfig
) {
  private val freshNameSupply: FreshNameSupply = new FreshNameSupply
  private val passes: Passes                   = new Passes(config)
  private val passManager: PassManager         = passes.passManager
  private val importResolver: ImportResolver   = new ImportResolver(this)
  private val irCachingEnabled                 = !context.isIrCachingDisabled
  private val useGlobalCacheLocations          = context.isUseGlobalCacheLocations
  private val isInteractiveMode                = context.isInteractiveMode
  private val output: PrintStream =
    if (config.outputRedirect.isDefined)
      new PrintStream(config.outputRedirect.get)
    else context.getOut

  /** Java accessor */
  def getConfig(): CompilerConfig = config

  /** The thread pool that handles parsing of modules. */
  private val pool: ExecutorService = if (config.parallelParsing) {
    new ThreadPoolExecutor(
      Compiler.startingThreadCount,
      Compiler.maximumThreadCount,
      Compiler.threadKeepalive,
      TimeUnit.SECONDS,
      new LinkedBlockingDeque[Runnable](),
      (runnable: Runnable) => {
        context.createThread(runnable)
      }
    )
  } else null

  /** Duplicates this compiler with a different config.
    * @param newConfig Configuration to be used in the duplicated Compiler.
    */
  def duplicateWithConfig(newConfig: CompilerConfig): Compiler = {
    new Compiler(
      context,
      packageRepository,
      newConfig
    )
  }

  /** Run the initialization sequence. */
  def initialize(): Unit = {
    context.initializeBuiltinsIr(
      this,
      irCachingEnabled,
      freshNameSupply,
      passes
    )
    packageRepository.initialize().left.foreach(reportPackageError)
  }

  /** @return the package repository instance. */
  def getPackageRepository: PackageRepository =
    context.getPackageRepository

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
    * @return future to track subsequent serialization of the library
    */
  def compile(
    shouldCompileDependencies: Boolean,
    useGlobalCacheLocations: Boolean,
    generateDocs: Boolean
  ): Future[java.lang.Boolean] = {
    getPackageRepository.getMainProjectPackage match {
      case None =>
        context.log(
          Level.SEVERE,
          "No package found in the compiler environment. Aborting."
        )
        CompletableFuture.completedFuture(false)
      case Some(pkg) =>
        val packageModule = packageRepository.getModuleMap.get(
          s"${pkg.namespace}.${pkg.normalizedName}.Main"
        )
        packageModule match {
          case None =>
            context.log(
              Level.SEVERE,
              "Could not find entry point for compilation in package [{0}.{1}]",
              Array(pkg.namespace, pkg.normalizedName)
            )
            CompletableFuture.completedFuture(false)
          case Some(m) =>
            context.log(
              Compiler.defaultLogLevel,
              s"Compiling the package [${pkg.namespace}.${pkg.normalizedName}] " +
              s"starting at the root [${m.getName}]."
            )

            val packageModules = packageRepository.freezeModuleMap.collect {
              case (name, mod)
                  if name.startsWith(
                    s"${pkg.namespace}.${pkg.normalizedName}"
                  ) =>
                mod
            }.toList

            runInternal(
              packageModules,
              generateCode = false,
              shouldCompileDependencies
            )

            if (generateDocs) {
              org.enso.compiler.dump.GenerateDocs
                .write(pkg, packageModules.asJava)
            }

            context.serializeLibrary(
              this,
              pkg.libraryName,
              useGlobalCacheLocations
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
    parseModule(
      module,
      irCachingEnabled && !context.isInteractive(module)
    )
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
        val pending =
          packageRepository.getPendingModules.toList
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
    modules.foreach(m =>
      try {
        parseModule(m, irCachingEnabled && !context.isInteractive(m))
      } catch {
        case e: Throwable =>
          context.log(
            Level.SEVERE,
            "Encountered a critical failure while parsing module",
            e
          )
          context.log(
            Level.SEVERE,
            "Contents of module {0}: {0}",
            m.getPath,
            m.getCharacters.toString
          )
      }
    )

    val requiredModules = modules.flatMap { module =>
      val isLoadedFromSource =
        (m: Module) => !context.wasLoadedFromCache(m) && !context.isSynthetic(m)
      val importedModules = runImportsAndExportsResolution(
        module,
        generateCode && context.wasLoadedFromCache(module)
      )
      if (
        shouldCompileDependencies &&
        context.wasLoadedFromCache(module) &&
        importedModules.exists(isLoadedFromSource)
      ) {
        val importedModulesLoadedFromSource = importedModules
          .filter(isLoadedFromSource)
          .map(context.getModuleName)
        context.log(
          Compiler.defaultLogLevel,
          "{0} imported module caches were invalided, forcing invalidation of {1}. [{2}]",
          Array(
            importedModulesLoadedFromSource.length,
            context.getModuleName(module).toString,
            importedModulesLoadedFromSource.take(10).mkString("", ",", "...")
          )
        )
        context.updateModule(module, _.invalidateCache())
        parseModule(module, irCachingEnabled && !context.isInteractive(module))
        importedModules
          .filter(isLoadedFromSource)
          .foreach(m => {
            if (m.getBindingsMap == null) {
              parseModule(m, irCachingEnabled && !context.isInteractive(module))
            }
          })
        runImportsAndExportsResolution(module, generateCode)
      } else {
        importedModules
      }
    }.distinct

    if (irCachingEnabled) {
      requiredModules.foreach { module =>
        ensureParsed(module, !context.isInteractive(module))
      }
    }
    requiredModules.foreach { module =>
      if (
        !context
          .getCompilationStage(module)
          .isAtLeast(
            CompilationStage.AFTER_GLOBAL_TYPES
          )
      ) {

        val moduleContext = ModuleContext(
          module          = module,
          freshNameSupply = Some(freshNameSupply),
          compilerConfig  = config
        )
        val compilerOutput =
          runGlobalTypingPasses(context.getIr(module), moduleContext)

        context.updateModule(
          module,
          { u =>
            u.ir(compilerOutput)
            u.compilationStage(CompilationStage.AFTER_GLOBAL_TYPES)
          }
        )
      }
    }
    requiredModules.foreach { module =>
      if (
        !context
          .getCompilationStage(module)
          .isAtLeast(
            CompilationStage.AFTER_STATIC_PASSES
          )
      ) {

        val moduleContext = ModuleContext(
          module          = module,
          freshNameSupply = Some(freshNameSupply),
          compilerConfig  = config,
          pkgRepo         = Some(packageRepository)
        )
        val compilerOutput =
          runMethodBodyPasses(context.getIr(module), moduleContext)
        context.updateModule(
          module,
          { u =>
            u.ir(compilerOutput)
            u.compilationStage(CompilationStage.AFTER_STATIC_PASSES)
          }
        )
      }
    }

    runErrorHandling(requiredModules)

    val requiredModulesWithScope = requiredModules.map { module =>
      if (
        !context
          .getCompilationStage(module)
          .isAtLeast(
            CompilationStage.AFTER_RUNTIME_STUBS
          )
      ) {
        val moduleScopeBuilder = module.getScopeBuilder()
        context.runStubsGenerator(module, moduleScopeBuilder)
        context.updateModule(
          module,
          { u =>
            u.compilationStage(CompilationStage.AFTER_RUNTIME_STUBS)
          }
        )
        (module, moduleScopeBuilder)
      } else {
        (module, module.getScopeBuilder)
      }
    }

    requiredModulesWithScope.foreach { case (module, moduleScopeBuilder) =>
      if (
        !context
          .getCompilationStage(module)
          .isAtLeast(
            CompilationStage.AFTER_CODEGEN
          )
      ) {

        if (generateCode) {
          context.log(
            Compiler.defaultLogLevel,
            "Generating code for module [{0}].",
            context.getModuleName(module)
          )

          context.truffleRunCodegen(module, moduleScopeBuilder, config)
        }
        context.updateModule(
          module,
          { u =>
            u.compilationStage(CompilationStage.AFTER_CODEGEN)
          }
        )

        if (
          shouldCompileDependencies || (!context.isInteractive(
            module
          ) && context.isModuleInRootPackage(module))
        ) {
          val shouldStoreCache =
            generateCode &&
            irCachingEnabled && !context.wasLoadedFromCache(module)
          if (
            shouldStoreCache && !hasErrors(module) &&
            !context.isInteractive(module) && !context.isSynthetic(module)
          ) {
            if (isInteractiveMode) {
              context.notifySerializeModule(context.getModuleName(module))
            } else {
              context.serializeModule(
                this,
                module,
                useGlobalCacheLocations,
                true
              )
            }
          }
        } else {
          context.log(
            Compiler.defaultLogLevel,
            "Skipping serialization for [{0}].",
            context.getModuleName(module)
          )
        }
      }
    }

    requiredModules
  }

  private def runImportsAndExportsResolution(
    module: Module,
    bindingsCachingEnabled: Boolean
  ): List[Module] = {
    val (importedModules, modulesImportedWithCachedBindings) =
      try {
        importResolver.mapImports(module, bindingsCachingEnabled)
      } catch {
        case e: ImportResolverAlgorithm.HiddenNamesConflict =>
          reportExportConflicts(e)
      }

    val requiredModules =
      try { new ExportsResolution(context).run(importedModules) }
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
      new ExportsResolution(context).runSort(modulesImportedWithCachedBindings)
    val allSortedModules = sortedCachedModules ++ requiredModules
    allSortedModules.foreach { mod =>
      val newModIr =
        ExportSymbolAnalysis.analyseModule(mod.getIr, packageRepository)
      context.updateModule(
        mod,
        updater => {
          updater.ir(newModIr)
        }
      )
    }
    allSortedModules
  }

  private def ensureParsedAndAnalyzed(module: Module): Unit = {
    if (module.getBindingsMap() == null) {
      ensureParsed(module, irCachingEnabled && !context.isInteractive(module))
    }
    if (context.isSynthetic(module)) {
      // Synthetic modules need to be import-analyzed
      // i.e. we need to fill in resolved{Imports/Exports} and exportedSymbols in bindings
      // because we do not generate (and deserialize) IR for them
      // TODO: consider generating IR for synthetic modules, if possible.
      importExportBindings(module) match {
        case Some(bindings) =>
          context.updateModule(
            module,
            _.ir(bindings)
          )
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
    ensureParsed(module, irCachingEnabled && !context.isInteractive(module))
    val importedModules = context.getIr(module).imports.flatMap {
      case imp: Import.Module =>
        imp.name.parts.take(2).map(_.name) match {
          case List(namespace, name) => List(LibraryName(namespace, name))
          case _ =>
            throw new CompilerError(s"Invalid module name: [${imp.name}].")
        }

      case _: imports.Polyglot =>
        // Note [Polyglot Imports In Dependency Gathering]
        Nil
      case other =>
        throw new CompilerError(
          s"Unexpected import type after processing ${context
            .getModuleName(module)}: [$other]."
        )
    }
    importedModules.distinct.map(_.qualifiedName).toArray
  }

  private def parseModule(
    module: Module,
    useCaches: Boolean
  ): Unit = {
    context.log(
      Compiler.defaultLogLevel,
      "Parsing module [{0}].",
      context.getModuleName(module)
    )
    context.updateModule(module, _.resetScope())

    if (
      useCaches && context.getIdMap(module) == null && context
        .deserializeModule(this, module)
    ) {
      return
    }

    uncachedParseModule(module)
  }

  /** Retrieve module bindings from cache, if available.
    *
    * @param module module which is conssidered
    * @return module's bindings, if available in libraries' bindings cache
    */
  def importExportBindings(module: Module): Option[IRModule] = {
    if (irCachingEnabled && !context.isInteractive(module)) {
      val libraryName = Option(module.getPackage).map(_.libraryName)
      libraryName.flatMap(
        packageRepository.getLibraryBindings(_, module.getName, context)
      )
    } else None
  }

  private def uncachedParseModule(module: Module): Unit = {
    context.log(
      Compiler.defaultLogLevel,
      "Loading module [{0}] from source.",
      context.getModuleName(module)
    )
    context.updateModule(module, _.resetScope())

    val moduleContext = ModuleContext(
      module          = module,
      freshNameSupply = Some(freshNameSupply),
      compilerConfig  = config
    )

    val src   = context.getCharacters(module)
    val idMap = Option(context.getIdMap(module))
    val expr  = EnsoParser.compile(src, idMap.map(_.values).orNull)

    val exprWithModuleExports =
      if (context.isSynthetic(module))
        expr
      else
        injectSyntheticModuleExports(expr, module.getDirectModulesRefs)
    val discoveredModule =
      recognizeBindings(exprWithModuleExports, moduleContext)
    if (context.wasLoadedFromCache(module)) {
      if (module.getBindingsMap() != null) {
        discoveredModule.passData.update(
          new MetadataPair(BindingAnalysis, module.getBindingsMap())
        )
      }
    }
    context.updateModule(
      module,
      { u =>
        u.bindingsMap(null);
        u.ir(discoveredModule)
        u.compilationStage(CompilationStage.AFTER_PARSING)
        u.loadedFromCache(false)
      }
    )
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
    Option(context.findTopScopeModule(name))
  }

  /** Ensures the passed module is in at least the parsed compilation stage.
    *
    * @param module the module to ensure is parsed.
    */
  def ensureParsed(module: Module): Unit = {
    val useCaches = irCachingEnabled && !context.isInteractive(module)
    ensureParsed(module, useCaches)
  }

  def ensureParsed(module: Module, useCaches: Boolean): Unit = {
    if (
      !context
        .getCompilationStage(module)
        .isAtLeast(
          CompilationStage.AFTER_PARSING
        )
    ) {
      parseModule(module, useCaches)
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
    srcString: CharSequence,
    inlineContext: InlineContext
  ): Option[(InlineContext, Expression)] = {
    val newContext = inlineContext.copy(freshNameSupply = Some(freshNameSupply))

    EnsoParser.compileInline(srcString).map { ir =>
      val compilerOutput = runCompilerPhasesInline(ir, newContext)
      runErrorHandlingInline(compilerOutput, newContext)
      (newContext, compilerOutput)
    }
  }

  /** Parses the given source with the new Rust parser.
    *
    * @param source The inline code to parse
    * @return A Tree representation of `source`
    */
  def parseInline(source: CharSequence): Tree =
    Parser.parseBlock(source)

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
    ir: IRModule,
    modules: java.util.List[QualifiedName]
  ): IRModule = {
    import scala.jdk.CollectionConverters._

    val moduleNames = modules.asScala.map { q =>
      val name = q.path.foldRight(
        List(Name.Literal(q.item, isMethod = false, identifiedLocation = null))
      ) { case (part, acc) =>
        Name.Literal(part, isMethod = false, identifiedLocation = null) :: acc
      }
      Name.Qualified(name, identifiedLocation = null)
    }.toList
    ir.copy(
      imports = ir.imports ::: moduleNames.map(m =>
        Import.Module(
          m,
          rename             = None,
          isAll              = false,
          onlyNames          = None,
          hiddenNames        = None,
          identifiedLocation = null,
          isSynthetic        = true
        )
      ),
      exports = ir.exports ::: moduleNames.map(m =>
        Export.Module(
          m,
          rename             = None,
          onlyNames          = None,
          identifiedLocation = null,
          isSynthetic        = true
        )
      )
    )
  }

  private def recognizeBindings(
    module: IRModule,
    moduleContext: ModuleContext
  ): IRModule = {
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
    ir: IRModule,
    moduleContext: ModuleContext
  ): IRModule = {
    context.log(
      Level.FINEST,
      "Passing module {0} with method body passes",
      moduleContext.module.getName
    )
    passManager.runPassesOnModule(ir, moduleContext, passes.functionBodyPasses)
  }

  private def runGlobalTypingPasses(
    ir: IRModule,
    moduleContext: ModuleContext
  ): IRModule = {
    context.log(
      Level.FINEST,
      "Passing module {0} with global typing passes",
      moduleContext.module.getName
    )
    passManager.runPassesOnModule(ir, moduleContext, passes.globalTypingPasses)
  }

  /** Runs the various compiler passes in an inline context.
    *
    * @param ir the compiler intermediate representation to transform
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return the output result of the
    */
  private def runCompilerPhasesInline(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    passManager.runPassesInline(ir, inlineContext)
  }

  /** Runs the strict error handling mechanism (if enabled in the language
    * context) for the inline compiler flow.
    *
    * @param ir the IR after compilation passes.
    * @param inlineContext the inline compilation context.
    */
  private def runErrorHandlingInline(
    ir: Expression,
    inlineContext: InlineContext
  ): Unit = {
    val errors = GatherDiagnostics
      .runExpression(ir, inlineContext)
      .unsafeGetMetadata(
        GatherDiagnostics,
        "No diagnostics metadata right after the gathering pass."
      )
      .diagnostics
    val module    = inlineContext.getModule()
    val hasErrors = reportDiagnostics(errors, module)
    hasErrors match {
      case error :: _ if inlineContext.compilerConfig.isStrictErrors =>
        throw error
      case _ =>
    }
  }

  /** Runs the strict error handling mechanism (if enabled in the language
    * context) for the module-level compiler flow.
    *
    * @param modules the modules to check against errors
    */
  private def runErrorHandling(
    modules: List[Module]
  ): Unit = {
    val diagnostics = modules.flatMap { module =>
      val errors =
        if (context.wasLoadedFromCache(module)) List()
        else gatherDiagnostics(module)
      List((module, errors))
    }

    val hasErrors = reportDiagnostics(diagnostics)
    if (hasErrors.nonEmpty && config.isStrictErrors) {
      val count =
        diagnostics.map(_._2.collect { case e: Error => e }.length).sum
      val warnCount =
        diagnostics.map(_._2.collect { case e: Warning => e }.length).sum
      context.getErr.println(
        s"Aborting due to ${count} errors and ${warnCount} warnings."
      )
      throw hasErrors.head
    }
  }

  /** Gathers diagnostics for a single module.
    *
    * @param module the module for which to gather diagnostics
    * @return the diagnostics from the module
    */
  private def gatherDiagnostics(module: Module): List[Diagnostic] = {
    GatherDiagnostics
      .runModule(
        context.getIr(module),
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
      case _: Error => true
      case _        => false
    }

  private def reportCycle(exception: ExportCycleException): Nothing = {
    printDiagnostic("Compiler encountered errors:")
    printDiagnostic("Export statements form a cycle:")
    exception.modules match {
      case List(mod) =>
        printDiagnostic(s"    ${mod.getName} exports itself.")
      case first :: second :: rest =>
        printDiagnostic(
          s"    ${first.getName} exports ${second.getName}"
        )
        rest.foreach { mod =>
          printDiagnostic(s"    which exports ${mod.getName}")
        }
        printDiagnostic(
          s"    which exports ${first.getName}, forming a cycle."
        )
      case _ =>
    }

    if (config.isStrictErrors) {
      throw context.throwAbortedException()
    } else {
      throw exception
    }
  }

  private def reportExportConflicts(exception: Throwable): Nothing = {
    printDiagnostic("Compiler encountered errors:")
    printDiagnostic(exception.getMessage)

    if (config.isStrictErrors) {
      throw context.throwAbortedException()
    } else {
      throw exception
    }
  }

  /** Report the errors encountered when initializing the package repository.
    *
    * @param err the package repository error
    */
  private def reportPackageError(err: PackageRepository.Error): Unit = {
    printDiagnostic(
      s"In package description ${org.enso.pkg.Package.configFileName}:"
    )
    printDiagnostic("Compiler encountered warnings:")
    printDiagnostic(err.toString)
  }

  /** Reports diagnostics from multiple modules.
    *
    * @param diagnostics the mapping between modules and existing diagnostics.
    * @return whether any errors were encountered.
    */
  private def reportDiagnostics(
    diagnostics: List[(Module, List[Diagnostic])]
  ): List[RuntimeException] = {
    diagnostics.flatMap { diags =>
      if (diags._2.nonEmpty) {
        reportDiagnostics(diags._2, diags._1)
      } else {
        List()
      }
    }
  }

  /** Reports compilation diagnostics to the standard output and throws an
    * exception breaking the execution flow if there are errors.
    *
    * @param diagnostics all the diagnostics found in the program IR.
    * @param compilerModule The module in which the diagnostics should be reported. Or null if run inline.
    * @return whether any errors were encountered.
    */
  private def reportDiagnostics(
    diagnostics: List[Diagnostic],
    compilerModule: CompilerContext.Module
  ): List[RuntimeException] = {
    val isOutputRedirected = config.outputRedirect.isDefined
    val exceptions = diagnostics
      .flatMap { diag =>
        val formattedDiag =
          context.formatDiagnostic(compilerModule, diag, isOutputRedirected)
        printDiagnostic(formattedDiag.getMessage)
        if (diag.isInstanceOf[Error]) {
          Some(formattedDiag)
        } else {
          None
        }
      }
    exceptions
  }

  /** Performs shutdown actions for the compiler.
    *
    * @param waitForPendingJobCompletion whether or not shutdown should wait for
    *                                    jobs to complete
    */
  def shutdown(waitForPendingJobCompletion: Boolean): Unit = {
    context.shutdown(waitForPendingJobCompletion)
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
  def updateMetadata(sourceIr: IRModule, copyOfIr: IRModule): IRModule = {
    passManager.runMetadataUpdate(sourceIr, copyOfIr)
  }

  private def printDiagnostic(message: String): Unit =
    if (config.isStrictErrors) output.println(message)
    else context.log(Level.WARNING, message)
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

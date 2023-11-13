package org.enso.compiler

import com.oracle.truffle.api.source.{Source, SourceSection}
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
  IdentifiedLocation,
  Name,
  Warning,
  Module => IRModule
}
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.module.scope.Export
import org.enso.compiler.core.ir.module.scope.Import
import org.enso.compiler.core.ir.module.scope.imports;
import org.enso.compiler.core.EnsoParser
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.exception.CompilationAbortedException
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse._
import org.enso.compiler.phase.{
  ExportCycleException,
  ExportsResolution,
  ImportResolver
}
import org.enso.editions.LibraryName
import org.enso.pkg.QualifiedName
import org.enso.polyglot.LanguageInfo
import org.enso.polyglot.CompilationStage
import org.enso.syntax2.Tree

import java.io.{PrintStream, StringReader}
import java.util.concurrent.{
  CompletableFuture,
  ExecutorService,
  Future,
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
  val context: CompilerContext,
  val packageRepository: PackageRepository,
  config: CompilerConfig
) {
  private val freshNameSupply: FreshNameSupply = new FreshNameSupply
  private val passes: Passes                   = new Passes(config)
  private val passManager: PassManager         = passes.passManager
  private val importResolver: ImportResolver   = new ImportResolver(this)
  private val irCachingEnabled                 = !context.isIrCachingDisabled
  private val useGlobalCacheLocations          = context.isUseGlobalCacheLocations
  private val isInteractiveMode                = context.isInteractiveMode()
  private val output: PrintStream =
    if (config.outputRedirect.isDefined)
      new PrintStream(config.outputRedirect.get)
    else context.getOut
  private lazy val ensoCompiler: EnsoParser = new EnsoParser()

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
  def getPackageRepository(): PackageRepository =
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
    useGlobalCacheLocations: Boolean
  ): Future[java.lang.Boolean] = {
    getPackageRepository().getMainProjectPackage match {
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
        parseModule(m)
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
            m.getSource.getCharacters.toString
          )
      }
    )

    var requiredModules = modules.flatMap { module =>
      val importedModules = runImportsAndExportsResolution(module, generateCode)
      val isLoadedFromSource =
        (m: Module) => !context.wasLoadedFromCache(m) && !context.isSynthetic(m)
      if (
        shouldCompileDependencies &&
        context.wasLoadedFromCache(module) &&
        importedModules.exists(isLoadedFromSource)
      ) {
        val importedModulesLoadedFromSource = importedModules
          .filter(isLoadedFromSource)
          .map(context.getModuleName(_))
        context.log(
          Compiler.defaultLogLevel,
          "{0} imported module caches were invalided, forcing invalidation of {1}. [{2}]",
          Array(
            importedModulesLoadedFromSource.length,
            context.getModuleName(module).toString,
            importedModulesLoadedFromSource.take(10).mkString("", ",", "...")
          )
        )
        context.updateModule(module, _.invalidateCache)
        parseModule(module)
        runImportsAndExportsResolution(module, generateCode)
      } else {
        importedModules
      }
    }.distinct

    var hasInvalidModuleRelink = false
    if (irCachingEnabled) {
      requiredModules.foreach { module =>
        ensureParsed(module)
        if (!context.hasCrossModuleLinks(module)) {
          val flags =
            context
              .getIr(module)
              .preorder
              .map(_.passData.restoreFromSerialization(this.context))

          if (!flags.contains(false)) {
            context.log(
              Compiler.defaultLogLevel,
              "Restored links (late phase) for module [{0}].",
              context.getModuleName(module)
            )
          } else {
            hasInvalidModuleRelink = true
            context.log(
              Compiler.defaultLogLevel,
              "Failed to restore links (late phase) for module [{0}].",
              context.getModuleName(module)
            )
            uncachedParseModule(module, isGenDocs = false)
          }
        }
      }
    }

    if (hasInvalidModuleRelink) {
      context.log(
        Compiler.defaultLogLevel,
        s"Some modules failed to relink. Re-running import and " +
        s"export resolution."
      )

      requiredModules =
        modules.flatMap(runImportsAndExportsResolution(_, generateCode))
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

    requiredModules.foreach { module =>
      if (
        !context
          .getCompilationStage(module)
          .isAtLeast(
            CompilationStage.AFTER_RUNTIME_STUBS
          )
      ) {
        context.runStubsGenerator(module)
        context.updateModule(
          module,
          { u =>
            u.compilationStage(CompilationStage.AFTER_RUNTIME_STUBS)
          }
        )
      }
    }
    requiredModules.foreach { module =>
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

          context.truffleRunCodegen(module, config)
        }
        context.updateModule(
          module,
          { u =>
            u.compilationStage(CompilationStage.AFTER_CODEGEN)
          }
        )

        if (shouldCompileDependencies || isModuleInRootPackage(module)) {
          val shouldStoreCache =
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
                useGlobalCacheLocations
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

  private def isModuleInRootPackage(module: Module): Boolean = {
    if (!context.isInteractive(module)) {
      val pkg = PackageRepositoryUtils
        .getPackageOf(getPackageRepository(), module.getSourceFile)
        .toScala
      pkg.contains(getPackageRepository().getMainProjectPackage.get)
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
    if (module.getBindingsMap() == null) {
      ensureParsed(module)
    }
    if (context.isSynthetic(module)) {
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
          ensureParsed(module)
          val currentLocal = module.getBindingsMap()
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
    isGenDocs: Boolean = false
  ): Unit = {
    context.log(
      Compiler.defaultLogLevel,
      "Parsing module [{0}].",
      context.getModuleName(module)
    )
    context.updateModule(module, _.resetScope)

    if (irCachingEnabled && !context.isInteractive(module)) {
      if (context.deserializeModule(this, module)) {
        return
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
    if (irCachingEnabled && !context.isInteractive(module)) {
      val libraryName = Option(module.getPackage).map(_.libraryName)
      libraryName.flatMap(
        packageRepository.getLibraryBindings(_, module.getName, context)
      )
    } else None
  }

  private def uncachedParseModule(module: Module, isGenDocs: Boolean): Unit = {
    context.log(
      Compiler.defaultLogLevel,
      "Loading module [{0}] from source.",
      context.getModuleName(module)
    )
    context.updateModule(module, _.resetScope)

    val moduleContext = ModuleContext(
      module           = module,
      freshNameSupply  = Some(freshNameSupply),
      compilerConfig   = config,
      isGeneratingDocs = isGenDocs
    )

    val src  = context.getCharacters(module)
    val tree = ensoCompiler.parse(src)
    val expr = ensoCompiler.generateIR(tree)

    val exprWithModuleExports =
      if (context.isSynthetic(module))
        expr
      else
        injectSyntheticModuleExports(expr, module.getDirectModulesRefs)
    val discoveredModule =
      recognizeBindings(exprWithModuleExports, moduleContext)
    context.updateModule(
      module,
      { u =>
        u.ir(discoveredModule)
        u.compilationStage(CompilationStage.AFTER_PARSING)
        u.loadedFromCache(false)
        u.hasCrossModuleLinks(true)
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
    if (
      !context
        .getCompilationStage(module)
        .isAtLeast(
          CompilationStage.AFTER_PARSING
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
  ): Option[(InlineContext, Expression, Source)] = {
    val newContext = inlineContext.copy(freshNameSupply = Some(freshNameSupply))
    val source = Source
      .newBuilder(
        LanguageInfo.ID,
        new StringReader(srcString),
        "<interactive_source>"
      )
      .build()
    val tree = ensoCompiler.parse(source.getCharacters)

    ensoCompiler.generateIRInline(tree).flatMap { ir =>
      val compilerOutput = runCompilerPhasesInline(ir, newContext)
      runErrorHandlingInline(compilerOutput, source, newContext)
      Some((newContext, compilerOutput, source))
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
    loc: Option[IdentifiedLocation],
    source: Source
  ): Unit = {
    val module = Option(context.findTopScopeModule(qualifiedName))
      .getOrElse {
        val locStr = fileLocationFromSection(loc, source)
        throw new CompilerError(
          s"Attempted to import the unresolved module $qualifiedName " +
          s"during code generation. Defined at $locStr."
        )
      }
    if (
      !module.getCompilationStage.isAtLeast(
        CompilationStage.AFTER_RUNTIME_STUBS
      )
    ) {
      throw new CompilerError(
        "Trying to use a module in codegen without generating runtime stubs"
      )
    }
  }

  /** Parses the given source with the new Rust parser.
    *
    * @param source The inline code to parse
    * @return A Tree representation of `source`
    */
  def parseInline(source: Source): Tree =
    ensoCompiler.parse(source.getCharacters())

  /** Parses the metadata of the provided language sources.
    *
    * @param source the code to parse
    * @return the source metadata
    */
//  def parseMeta(source: CharSequence): IDMap =
//    Parser().splitMeta(source.toString)._2

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
        List(Name.Literal(q.item, isMethod = false, location = None))
      ) { case (part, acc) =>
        Name.Literal(part, isMethod = false, location = None) :: acc
      }
      Name.Qualified(name, location = None)
    }.toList
    ir.copy(
      imports = ir.imports ::: moduleNames.map(m =>
        Import.Module(
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
        Export.Module(
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
    passManager.runPassesOnModule(ir, moduleContext, passes.functionBodyPasses)
  }

  private def runGlobalTypingPasses(
    ir: IRModule,
    moduleContext: ModuleContext
  ): IRModule = {
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
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
    ir: Expression,
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
          diagnostics.map(_._2.collect { case e: Error => e }.length).sum
        val warnCount =
          diagnostics.map(_._2.collect { case e: Warning => e }.length).sum
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
  def gatherDiagnostics(module: Module): List[Diagnostic] = {
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
    diagnostics: List[(Module, List[Diagnostic])]
  ): Boolean = {
    // It may be tempting to replace `.foldLeft(..)` with
    // `.find(...).nonEmpty. Don't. We want to report diagnostics for all modules
    // not just the first one.
    diagnostics
      .foldLeft(false) { case (result, (mod, diags)) =>
        if (diags.nonEmpty) {
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
    diagnostics: List[Diagnostic],
    source: Source
  ): Boolean = {
    diagnostics.foreach(diag =>
      output.println(new DiagnosticFormatter(diag, source).format())
    )
    diagnostics.exists(_.isInstanceOf[Error])
  }

  /** Formatter of IR diagnostics. Heavily inspired by GCC. Can format one-line as well as multiline
    * diagnostics. The output is colorized if the output stream supports ANSI colors.
    * Also prints the offending lines from the source along with line number - the same way as
    * GCC does.
    * @param diagnostic the diagnostic to pretty print
    * @param source     the original source code
    */
  private class DiagnosticFormatter(
    private val diagnostic: Diagnostic,
    private val source: Source
  ) {
    private val maxLineNum                     = 99999
    private val blankLinePrefix                = "      | "
    private val maxSourceLinesToPrint          = 3
    private val linePrefixSize                 = blankLinePrefix.length
    private val outSupportsAnsiColors: Boolean = outSupportsColors
    private val (textAttrs: fansi.Attrs, subject: String) = diagnostic match {
      case _: Error   => (fansi.Color.Red ++ fansi.Bold.On, "error: ")
      case _: Warning => (fansi.Color.Yellow ++ fansi.Bold.On, "warning: ")
      case _          => throw new IllegalStateException("Unexpected diagnostic type")
    }
    private val sourceSection: Option[SourceSection] =
      diagnostic.location match {
        case Some(location) =>
          Some(source.createSection(location.start, location.length))
        case None => None
      }
    private val shouldPrintLineNumber = sourceSection match {
      case Some(section) =>
        section.getStartLine <= maxLineNum && section.getEndLine <= maxLineNum
      case None => false
    }

    def format(): String = {
      sourceSection match {
        case Some(section) =>
          val isOneLine = section.getStartLine == section.getEndLine
          val srcPath: String =
            if (source.getPath == null && source.getName == null) {
              "<Unknown source>"
            } else if (source.getPath != null) {
              source.getPath
            } else {
              source.getName
            }
          if (isOneLine) {
            val lineNumber  = section.getStartLine
            val startColumn = section.getStartColumn
            val endColumn   = section.getEndColumn
            var str         = fansi.Str()
            str ++= fansi
              .Str(srcPath + ":" + lineNumber + ":" + startColumn + ": ")
              .overlay(fansi.Bold.On)
            str ++= fansi.Str(subject).overlay(textAttrs)
            str ++= diagnostic.formattedMessage(source)
            str ++= "\n"
            str ++= oneLineFromSourceColored(lineNumber, startColumn, endColumn)
            str ++= "\n"
            str ++= underline(startColumn, endColumn)
            if (outSupportsAnsiColors) {
              str.render.stripLineEnd
            } else {
              str.plainText.stripLineEnd
            }
          } else {
            var str = fansi.Str()
            str ++= fansi
              .Str(
                srcPath + ":[" + section.getStartLine + ":" + section.getStartColumn + "-" + section.getEndLine + ":" + section.getEndColumn + "]: "
              )
              .overlay(fansi.Bold.On)
            str ++= fansi.Str(subject).overlay(textAttrs)
            str ++= diagnostic.formattedMessage(source)
            str ++= "\n"
            val printAllSourceLines =
              section.getEndLine - section.getStartLine <= maxSourceLinesToPrint
            val endLine =
              if (printAllSourceLines) section.getEndLine
              else section.getStartLine + maxSourceLinesToPrint
            for (lineNum <- section.getStartLine to endLine) {
              str ++= oneLineFromSource(lineNum)
              str ++= "\n"
            }
            if (!printAllSourceLines) {
              val restLineCount =
                section.getEndLine - section.getStartLine - maxSourceLinesToPrint
              str ++= blankLinePrefix + "... and " + restLineCount + " more lines ..."
              str ++= "\n"
            }
            if (outSupportsAnsiColors) {
              str.render.stripLineEnd
            } else {
              str.plainText.stripLineEnd
            }
          }
        case None =>
          // There is no source section associated with the diagnostics
          var str = fansi.Str()
          val fileLocation = diagnostic.location match {
            case Some(_) => fileLocationFromSection(diagnostic.location, source)
            case None    => source.getPath
          }
          str ++= fansi
            .Str(fileLocation)
            .overlay(fansi.Bold.On)
          str ++= ": "
          str ++= fansi.Str(subject).overlay(textAttrs)
          str ++= diagnostic.formattedMessage(source)
          if (outSupportsAnsiColors) {
            str.render.stripLineEnd
          } else {
            str.plainText.stripLineEnd
          }
      }
    }

    /** @see https://github.com/termstandard/colors/
      * @see https://no-color.org/
      * @return
      */
    private def outSupportsColors: Boolean = {
      if (System.console() == null) {
        // Non-interactive output is always without color support
        return false
      }
      if (System.getenv("NO_COLOR") != null) {
        return false
      }
      if (config.outputRedirect.isDefined) {
        return false
      }
      if (System.getenv("COLORTERM") != null) {
        return true
      }
      if (System.getenv("TERM") != null) {
        val termEnv = System.getenv("TERM").toLowerCase
        return termEnv.split("-").contains("color") || termEnv
          .split("-")
          .contains("256color")
      }
      return false
    }

    private def oneLineFromSource(lineNum: Int): String = {
      val line = source.createSection(lineNum).getCharacters.toString
      linePrefix(lineNum) + line
    }

    private def oneLineFromSourceColored(
      lineNum: Int,
      startCol: Int,
      endCol: Int
    ): String = {
      val line = source.createSection(lineNum).getCharacters.toString
      linePrefix(lineNum) + fansi
        .Str(line)
        .overlay(textAttrs, startCol - 1, endCol)
    }

    private def linePrefix(lineNum: Int): String = {
      if (shouldPrintLineNumber) {
        val pipeSymbol = " | "
        val prefixWhitespaces =
          linePrefixSize - lineNum.toString.length - pipeSymbol.length
        " " * prefixWhitespaces + lineNum + pipeSymbol
      } else {
        blankLinePrefix
      }
    }

    private def underline(startColumn: Int, endColumn: Int): String = {
      val sectionLen = endColumn - startColumn
      blankLinePrefix +
      " " * (startColumn - 1) +
      fansi.Str("^" + ("~" * sectionLen)).overlay(textAttrs)
    }
  }

  private def fileLocationFromSection(
    loc: Option[IdentifiedLocation],
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
    source.getPath + ":" + srcLocation
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

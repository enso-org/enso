package org.enso.compiler

import com.oracle.truffle.api.TruffleLogger
import com.oracle.truffle.api.source.Source
import org.enso.compiler.codegen.{AstToIr, IrToTruffle, RuntimeStubsGenerator}
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Expression
import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.exception.{CompilationAbortedException, CompilerError}
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse._
import org.enso.compiler.phase.{
  ExportCycleException,
  ExportsResolution,
  ImportResolver
}
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}
import org.enso.interpreter.runtime.{Context, Module}
import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.enso.syntax.text.Parser.IDMap
import org.enso.syntax.text.{AST, Parser}

import java.io.StringReader
import java.util.logging.Level
import scala.jdk.OptionConverters._

/** This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  *
  * @param context the language context
  */
class Compiler(
  val context: Context,
  val builtins: Builtins,
  val packageRepository: PackageRepository,
  config: CompilerConfig
) {
  private val freshNameSupply: FreshNameSupply = new FreshNameSupply
  private val passes: Passes                   = new Passes(config)
  private val passManager: PassManager         = passes.passManager
  private val importResolver: ImportResolver   = new ImportResolver(this)
  private val stubsGenerator: RuntimeStubsGenerator =
    new RuntimeStubsGenerator()
  private val irCachingEnabled = !context.isIrCachingDisabled
  private val useGlobalCacheLocations = context.getEnvironment.getOptions.get(
    RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION_KEY
  )
  private val serializationManager: SerializationManager =
    new SerializationManager(this)
  private val logger: TruffleLogger = context.getLogger(getClass)

  /** Lazy-initializes the IR for the builtins module.
    */
  def initializeBuiltinsIr(): Unit = {
    if (!builtins.isIrInitialized) {
      logger.log(
        Compiler.defaultLogLevel,
        s"Initialising IR for [${builtins.getModule.getName}]."
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
        serializationManager.serialize(
          builtins.getModule,
          useGlobalCacheLocations = true // Builtins can't have a local cache.
        )
      }
    }
  }

  /** Runs the import resolver on the given module.
    *
    * @param module the entry module for import resolution
    * @return the list of modules imported by `module`
    */
  def runImportsResolution(module: Module): List[Module] = {
    initializeBuiltinsIr()
    importResolver.mapImports(module)
  }

  /** Processes the provided language sources, registering any bindings in the
    * given scope.
    *
    * @param module the scope into which new bindings are registered
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(module: Module): Unit = {
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
    */
  def compile(shouldCompileDependencies: Boolean): Unit = {
    val packageRepository = context.getPackageRepository

    packageRepository.getMainProjectPackage match {
      case None =>
        logger.log(
          Level.SEVERE,
          s"No package found in the compiler environment. Aborting."
        )
      case Some(pkg) =>
        val packageModule = packageRepository.getModuleMap.get(
          s"${pkg.namespace}.${pkg.name}.Main"
        )
        packageModule match {
          case None =>
            logger.log(
              Level.SEVERE,
              s"Could not find entry point for compilation in package " +
              s"[${pkg.namespace}.${pkg.name}]."
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
        }
    }
  }

  /** Runs part of the compiler to generate docs from Enso code.
    *
    * @param module the scope from which docs are generated
    */
  def generateDocs(module: Module): Module = {
    initializeBuiltinsIr()
    parseModule(module, isGenDocs = true)
    module
  }

  private def runInternal(
    modules: List[Module],
    generateCode: Boolean,
    shouldCompileDependencies: Boolean
  ): Unit = {
    initializeBuiltinsIr()
    modules.foreach(m => parseModule(m))

    var requiredModules = modules.flatMap(runImportsAndExportsResolution)

    var hasInvalidModuleRelink = false
    if (irCachingEnabled) {
      requiredModules.foreach { module =>
        if (!module.hasCrossModuleLinks) {
          val flags =
            module.getIr.preorder.map(_.passData.restoreFromSerialization(this))

          if (!flags.contains(false)) {
            logger.log(
              Compiler.defaultLogLevel,
              s"Restored links (late phase) for module [${module.getName}]."
            )
          } else {
            hasInvalidModuleRelink = true
            logger.log(
              Compiler.defaultLogLevel,
              s"Failed to restore links (late phase) for module " +
              s"[${module.getName}]."
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

      requiredModules = modules.flatMap(runImportsAndExportsResolution)
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
          compilerConfig  = config
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
            s"Generating code for module [${module.getName}]."
          )

          truffleCodegen(module.getIr, module.getSource, module.getScope)
        }
        module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_CODEGEN)

        if (shouldCompileDependencies || isModuleInRootPackage(module)) {
          val shouldStoreCache =
            irCachingEnabled && !module.wasLoadedFromCache()
          if (shouldStoreCache && !hasErrors(module) && !module.isInteractive) {
            serializationManager.serialize(module, useGlobalCacheLocations)
          }
        } else {
          logger.log(
            Compiler.defaultLogLevel,
            s"Skipping serialization for [${module.getName}]."
          )
        }
      }
    }
  }

  private def isModuleInRootPackage(module: Module): Boolean = {
    if (!module.isInteractive) {
      val pkg = context.getPackageOf(module.getSourceFile).toScala
      pkg.contains(context.getPackageRepository.getMainProjectPackage.get)
    } else false
  }

  private def runImportsAndExportsResolution(module: Module): List[Module] = {
    val importedModules = importResolver.mapImports(module)

    val requiredModules =
      try { new ExportsResolution().run(importedModules) }
      catch { case e: ExportCycleException => reportCycle(e) }

    requiredModules
  }

  private def parseModule(
    module: Module,
    isGenDocs: Boolean = false
  ): Unit = {
    logger.log(
      Compiler.defaultLogLevel,
      s"Parsing the module [${module.getName}]."
    )
    module.ensureScopeExists()
    module.getScope.reset()

    if (irCachingEnabled && !module.isInteractive) {
      serializationManager.deserialize(module) match {
        case Some(_) => return
        case _       =>
      }
    }

    uncachedParseModule(module, isGenDocs)
  }

  private def uncachedParseModule(module: Module, isGenDocs: Boolean): Unit = {
    logger.log(
      Compiler.defaultLogLevel,
      s"Loading module `${module.getName}` from source."
    )
    module.ensureScopeExists()
    module.getScope.reset()

    val moduleContext = ModuleContext(
      module           = module,
      freshNameSupply  = Some(freshNameSupply),
      compilerConfig   = config,
      isGeneratingDocs = isGenDocs
    )
    val parsedAST        = parse(module.getSource)
    val expr             = generateIR(parsedAST)
    val discoveredModule = recognizeBindings(expr, moduleContext)
    module.unsafeSetIr(discoveredModule)
    module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_PARSING)
    module.setHasCrossModuleLinks(true)
  }

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
    val parsed: AST = parse(source)

    generateIRInline(parsed).flatMap { ir =>
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

  /** Parses the provided language sources.
    *
    * @param source the code to parse
    * @return an AST representation of `source`
    */
  def parse(source: Source): AST =
    Parser().runWithIds(source.getCharacters.toString)

  /** Parses the metadata of the provided language sources.
    *
    * @param source the code to parse
    * @return the source metadata
    */
  def parseMeta(source: CharSequence): IDMap =
    Parser().splitMeta(source.toString)._2

  /** Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST input
    * @return an IR representation of the program represented by `sourceAST`
    */
  def generateIR(sourceAST: AST): IR.Module =
    AstToIr.translate(sourceAST)

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

  /** Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST representing the program source
    * @return an IR representation of the program represented by `sourceAST`
    */
  def generateIRInline(sourceAST: AST): Option[Expression] =
    AstToIr.translateInline(sourceAST)

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
    if (context.isStrictErrors) {
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
    if (context.isStrictErrors) {
      val diagnostics = modules.flatMap { module =>
        if (module == builtins.getModule) {
          List()
        } else {
          val errors = gatherDiagnostics(module)
          List((module, errors))
        }
      }
      if (reportDiagnostics(diagnostics)) {
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
    if (context.isStrictErrors) {
      context.getOut.println("Compiler encountered errors:")
      context.getOut.println("Export statements form a cycle:")
      exception.modules match {
        case List(mod) =>
          context.getOut.println(s"    ${mod.getName} exports itself.")
        case first :: second :: rest =>
          context.getOut.println(
            s"    ${first.getName} exports ${second.getName}"
          )
          rest.foreach { mod =>
            context.getOut.println(s"    which exports ${mod.getName}")
          }
          context.getOut.println(
            s"    which exports ${first.getName}, forming a cycle."
          )
        case _ =>
      }
      throw new CompilationAbortedException
    } else {
      throw exception
    }
  }

  /** Reports diagnostics from multiple modules.
    *
    * @param diagnostics the mapping between modules and existing diagnostics.
    * @return whether any errors were encountered.
    */
  def reportDiagnostics(
    diagnostics: List[(Module, List[IR.Diagnostic])]
  ): Boolean = {
    val results = diagnostics.map { case (mod, diags) =>
      if (diags.nonEmpty) {
        context.getOut.println(s"In module ${mod.getName}:")
        reportDiagnostics(diags, mod.getSource)
      } else {
        false
      }
    }
    results.exists(r => r)
  }

  /** Reports compilation diagnostics to the standard output and throws an
    * exception breaking the execution flow if there are errors.
    *
    * @param diagnostics all the diagnostics found in the program IR.
    * @param source the original source code.
    * @return whether any errors were encountered.
    */
  def reportDiagnostics(
    diagnostics: List[IR.Diagnostic],
    source: Source
  ): Boolean = {
    val errors   = diagnostics.collect { case e: IR.Error => e }
    val warnings = diagnostics.collect { case w: IR.Warning => w }

    if (warnings.nonEmpty) {
      context.getOut.println("Compiler encountered warnings:")
      warnings.foreach { warning =>
        context.getOut.println(formatDiagnostic(warning, source))
      }
    }

    if (errors.nonEmpty) {
      context.getOut.println("Compiler encountered errors:")
      errors.foreach { error =>
        context.getOut.println(formatDiagnostic(error, source))
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
}

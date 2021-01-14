package org.enso.compiler

import java.io.StringReader
import com.oracle.truffle.api.source.Source
import org.enso.compiler.codegen.{AstToIr, IrToTruffle, RuntimeStubsGenerator}
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Expression
import org.enso.compiler.exception.{CompilationAbortedException, CompilerError}
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse._
import org.enso.compiler.phase.{
  ExportCycleException,
  ExportsResolution,
  ImportResolver
}
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.error.ModuleDoesNotExistException
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.builtin.Builtins
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}
import org.enso.polyglot.LanguageInfo
import org.enso.syntax.text.Parser.IDMap
import org.enso.syntax.text.{AST, Parser}

import scala.jdk.OptionConverters._

/** This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  *
  * @param context the language context
  */
class Compiler(val context: Context, private val builtins: Builtins) {
  private val freshNameSupply: FreshNameSupply = new FreshNameSupply
  private val passes: Passes                   = new Passes
  private val passManager: PassManager         = passes.passManager
  private val importResolver: ImportResolver   = new ImportResolver(this)
  private val stubsGenerator: RuntimeStubsGenerator =
    new RuntimeStubsGenerator()

  /** Lazy-initializes the IR for the builtins module.
    */
  def initializeBuiltinsIr(): Unit = {
    if (!builtins.isIrInitialized) {
      builtins.initializedBuiltinsIr(freshNameSupply, passes)
    }
  }

  /** Processes the provided language sources, registering any bindings in the
    * given scope.
    *
    * @param module the scope into which new bindings are registered
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(module: Module): Unit = {
    parseModule(module)
    val importedModules = importResolver.mapImports(module)
    val requiredModules =
      try { new ExportsResolution().run(importedModules) }
      catch { case e: ExportCycleException => reportCycle(e) }
    requiredModules.foreach { module =>
      if (
        !module.getCompilationStage.isAtLeast(
          Module.CompilationStage.AFTER_STATIC_PASSES
        )
      ) {

        val moduleContext = ModuleContext(
          module          = module,
          freshNameSupply = Some(freshNameSupply)
        )
        val compilerOutput = runCompilerPhases(module.getIr, moduleContext)
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
        truffleCodegen(module.getIr, module.getSource, module.getScope)
        module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_CODEGEN)
      }
    }
  }

  private def parseModule(module: Module): Unit = {
    module.ensureScopeExists()
    module.getScope.reset()
    val moduleContext = ModuleContext(
      module          = module,
      freshNameSupply = Some(freshNameSupply)
    )
    val parsedAST        = parse(module.getSource)
    val expr             = generateIR(parsedAST)
    val discoveredModule = recognizeBindings(expr, moduleContext)
    module.unsafeSetIr(discoveredModule)
    module.unsafeSetCompilationStage(Module.CompilationStage.AFTER_PARSING)
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
    * @return the scope containing all definitions in the requested module
    */
  def processImport(qualifiedName: String): ModuleScope = {
    val module = context.getTopScope
      .getModule(qualifiedName)
      .toScala
      .getOrElse(throw new ModuleDoesNotExistException(qualifiedName))
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
  def runCompilerPhases(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    passManager.runPassesOnModule(ir, moduleContext, passes.functionBodyPasses)
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
      val diagnostics = modules.map { module =>
        val errors = GatherDiagnostics
          .runModule(module.getIr, new ModuleContext(module))
          .unsafeGetMetadata(
            GatherDiagnostics,
            "No diagnostics metadata right after the gathering pass."
          )
          .diagnostics
        (module, errors)
      }
      if (reportDiagnostics(diagnostics)) {
        throw new CompilationAbortedException
      }
    }
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
    val srcLocation = diagnostic.location
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
    source.getName + srcLocation + ": " + diagnostic.message
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
    new IrToTruffle(context, source, scope).run(ir)
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
      inlineContext.module.getScope
    ).runInline(
      ir,
      inlineContext.localScope.getOrElse(LocalScope.root),
      "<inline_source>"
    )
  }
}

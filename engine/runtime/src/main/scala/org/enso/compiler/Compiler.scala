package org.enso.compiler

import java.io.StringReader

import com.oracle.truffle.api.TruffleFile
import com.oracle.truffle.api.source.Source
import org.enso.compiler.codegen.{AstToIR, IRToTruffle}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{Expression, Module}
import org.enso.compiler.exception.{CompilationAbortedException, CompilerError}
import org.enso.compiler.pass.IRPass

import org.enso.compiler.pass.analyse._

import org.enso.compiler.pass.desugar.{
  GenerateMethodBodies,
  LiftSpecialOperators,
  OperatorToFunction
}
import org.enso.interpreter.Language
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.runtime.error.ModuleDoesNotExistException
import org.enso.interpreter.runtime.scope.{
  LocalScope,
  ModuleScope,
  TopLevelScope
}
import org.enso.polyglot.LanguageInfo
import org.enso.syntax.text.{AST, Parser}

/**
  * This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  */
class Compiler(
  val language: Language,
  val topScope: TopLevelScope,
  val context: Context
) {

  /** A list of the compiler phases, in the order they should be run.
    *
    * Please note that these passes _must_ be run in this order. While we
    * currently can't account for the dependencies between passes in the types,
    * they nevertheless exist.
    */
  val compilerPhaseOrdering: List[IRPass] = List(
    GenerateMethodBodies,
    LiftSpecialOperators,
    OperatorToFunction,
    AliasAnalysis,
    DemandAnalysis,
    ApplicationSaturation(),
    TailCall,
    DataflowAnalysis
  )

  /**
    * Processes the provided language sources, registering any bindings in the
    * given scope.
    *
    * @param source the source code to be processed
    * @param scope the scope into which new bindings are registered
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(source: Source, scope: ModuleScope): Unit = {
    val parsedAST      = parse(source)
    val expr           = generateIR(parsedAST)
    val compilerOutput = runCompilerPhases(expr)
    runErrorHandling(compilerOutput, source)
    truffleCodegen(compilerOutput, source, scope)
  }

  /**
    * Processes the language sources in the provided file, registering any
    * bindings in the given scope.
    *
    * @param file the file containing the source code
    * @param scope the scope into which new bindings are registered
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(file: TruffleFile, scope: ModuleScope): Unit = {
    run(Source.newBuilder(LanguageInfo.ID, file).build, scope)
  }

  /**
    * Processes the provided language sources, registering their bindings in a
    * new scope.
    *
    * @param source the source code to be processed
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(source: Source, moduleName: String): Unit = {
    run(source, context.createScope(moduleName))
  }

  /**
    * Processes the language sources in the provided file, registering any
    * bindings in a new scope.
    *
    * @param file the file containing the source code
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(file: TruffleFile, moduleName: String): Unit = {
    run(Source.newBuilder(LanguageInfo.ID, file).build, moduleName)
  }

  /**
    * Processes the language source, interpreting it as an expression.
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
    val source = Source
      .newBuilder(
        LanguageInfo.ID,
        new StringReader(srcString),
        "<interactive_source>"
      )
      .build()
    val parsed: AST = parse(source)

    generateIRInline(parsed).flatMap { ir =>
      val compilerOutput = runCompilerPhasesInline(ir, inlineContext)
      runErrorHandlingInline(compilerOutput, source, inlineContext)
      Some(truffleCodegenInline(compilerOutput, source, inlineContext))
    }
  }

  /**
    * Finds and processes a language source by its qualified name.
    *
    * The results of this operation are cached internally so we do not need to
    * process the same source file multiple times.
    *
    * @param qualifiedName the qualified name of the module
    * @return the scope containing all definitions in the requested module
    */
  def processImport(qualifiedName: String): ModuleScope = {
    val module = topScope.getModule(qualifiedName)
    if (module.isPresent) {
      module.get().getScope(context)
    } else {
      throw new ModuleDoesNotExistException(qualifiedName)
    }
  }

  /**
    * Parses the provided language sources.
    *
    * @param source the code to parse
    * @return an AST representation of `source`
    */
  def parse(source: Source): AST =
    Parser().runWithIds(source.getCharacters.toString)

  /**
    * Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST input
    * @return an IR representation of the program represented by `sourceAST`
    */
  def generateIR(sourceAST: AST): Module =
    AstToIR.translate(sourceAST)

  /**
    * Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST representing the program source
    * @return an IR representation of the program represented by `sourceAST`
    */
  def generateIRInline(sourceAST: AST): Option[Expression] =
    AstToIR.translateInline(sourceAST)

  /** Runs the various compiler passes.
    *
    * @param ir the compiler intermediate representation to transform
    * @return the output result of the
    */
  def runCompilerPhases(ir: IR.Module): IR.Module = {
    compilerPhaseOrdering.foldLeft(ir)((intermediateIR, pass) =>
      pass.runModule(intermediateIR)
    )
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
    compilerPhaseOrdering.foldLeft(ir)((intermediateIR, pass) =>
      pass.runExpression(intermediateIR, inlineContext)
    )
  }

  /**
    * Runs the strict error handling mechanism (if enabled in the language
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
  ): Unit = if (context.isStrictErrors) {
    val errors = GatherErrors
      .runExpression(ir, inlineContext)
      .unsafeGetMetadata[GatherErrors.Errors](
        "No errors metadata right after the gathering pass."
      )
      .errors
    reportErrors(errors, source)
  }

  /**
    * Runs the strict error handling mechanism (if enabled in the language
    * context) for the module-level compiler flow.
    *
    * @param ir the IR after compilation passes.
    * @param source the original source code.
    */
  def runErrorHandling(ir: IR.Module, source: Source): Unit =
    if (context.isStrictErrors) {
      val errors = GatherErrors
        .runModule(ir)
        .unsafeGetMetadata[GatherErrors.Errors](
          "No errors metadata right after the gathering pass."
        )
        .errors
      reportErrors(errors, source)
    }

  /**
    * Reports compilation errors to the standard output and throws an exception
    * breaking the execution flow.
    *
    * @param errors all the errors found in the program IR.
    * @param source the original source code.
    */
  def reportErrors(errors: List[IR.Error], source: Source): Unit =
    if (errors.nonEmpty) {
      context.getOut.println("Compiler encountered errors:")
      errors.foreach { err =>
        val srcLocation = err.location
          .map { loc =>
            val section = source
              .createSection(loc.location.start, loc.location.length)
            val locStr =
              "" + section.getStartLine + ":" +
              section.getStartColumn + "-" +
              section.getEndLine + ":" +
              section.getEndColumn
            "[" + locStr + "]"
          }
          .getOrElse("")
        val fullMsg = source.getName + srcLocation + ": " + err.message
        context.getOut.println(fullMsg)
      }
      throw new CompilationAbortedException
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
    new IRToTruffle(context, source, scope).run(ir)
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
    new IRToTruffle(
      context,
      source,
      inlineContext.moduleScope.getOrElse(
        throw new CompilerError(
          "Cannot perform inline codegen with a missing module scope."
        )
      )
    ).runInline(
      ir,
      inlineContext.localScope.getOrElse(LocalScope.root),
      "<inline_source>"
    )
  }
}

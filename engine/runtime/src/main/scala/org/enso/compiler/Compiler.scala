package org.enso.compiler

import java.util.Optional

import com.oracle.truffle.api.TruffleFile
import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.{AstExpression, AstModuleScope, EnsoParser}
import org.enso.compiler.generate.AstToAstExpression
import org.enso.flexer.Reader
import org.enso.interpreter.builder.ExpressionFactory
import org.enso.interpreter.builder.ModuleScopeExpressionFactory
import org.enso.interpreter.node.ExpressionNode
import org.enso.interpreter.runtime.{Context, Module}
import org.enso.interpreter.runtime.callable.function.Function
import org.enso.interpreter.runtime.error.ModuleDoesNotExistException
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}
import org.enso.interpreter.{Constants, Language}
import org.enso.syntax.text.{AST, Parser}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * This class encapsulates the static transformation processes that take place
  * on source code, including parsing, desugaring, type-checking, static
  * analysis, and optimisation.
  */
class Compiler(
  val language: Language,
  val files: java.util.Map[String, Module],
  val context: Context
) {

  val knownFiles: mutable.Map[String, Module] = files.asScala

  /**
    * Processes the provided language sources, registering any bindings in the
    * given scope.
    *
    * @param source the source code to be processed
    * @param scope the scope into which new bindings are registered
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(source: Source, scope: ModuleScope): Optional[Function] = {
    val mimeType = source.getMimeType

    val expr: AstModuleScope = if (mimeType == Constants.MIME_TYPE) {
      val parsedAST: AST = parse(source)
      translate(parsedAST)
    } else {
      new EnsoParser().parseEnso(source.getCharacters.toString)
    }

    new ModuleScopeExpressionFactory(language, source, scope).run(expr)
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
  def run(file: TruffleFile, scope: ModuleScope): Optional[Function] = {
    run(Source.newBuilder(Constants.LANGUAGE_ID, file).build, scope)
  }

  /**
    * Processes the provided language sources, registering their bindings in a
    * new scope.
    *
    * @param source the source code to be processed
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(source: Source): Optional[Function] = {
    run(source, context.createScope)
  }

  /**
    * Processes the language sources in the provided file, registering any
    * bindings in a new scope.
    *
    * @param file the file containing the source code
    * @return an interpreter node whose execution corresponds to the top-level
    *         executable functionality in the module corresponding to `source`.
    */
  def run(file: TruffleFile): Optional[Function] = {
    run(Source.newBuilder(Constants.LANGUAGE_ID, file).build)
  }

  /**
    * Processes the language source, interpreting it as an expression.
    * Processes the source in the context of given local and module scopes.
    *
    * @param source string representing the expression to process
    * @param language current language instance
    * @param localScope local scope to process the source in
    * @param moduleScope module scope to process the source in
    * @return an expression node representing the parsed and analyzed source
    */
  def runInline(
    source: String,
    language: Language,
    localScope: LocalScope,
    moduleScope: ModuleScope
  ): ExpressionNode = {
    val parsed = parseInline(source)
    new ExpressionFactory(
      language,
      Source
        .newBuilder(Constants.LANGUAGE_ID, source, "<interactive_source>")
        .build(),
      localScope,
      "<inline_source>",
      moduleScope
    ).run(parsed)
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
  def requestProcess(qualifiedName: String): ModuleScope = {
    knownFiles.get(qualifiedName) match {
      case Some(module) => module.requestParse(language.getCurrentContext)
      case None         => throw new ModuleDoesNotExistException(qualifiedName)
    }
  }

  /**
    * Parses the provided language sources.
    *
    * @param source the code to parse
    * @return an AST representation of `source`
    */
  def parse(source: Source): AST = {
    val parser: Parser = Parser()
    val unresolvedAST: AST.Module =
      parser.run(new Reader(source.getCharacters.toString))
    val resolvedAST: AST.Module = parser.dropMacroMeta(unresolvedAST)

    resolvedAST
  }

  /**
    * Parses the provided language source expression in inline mode.
    *
    * @param source the code to parse
    * @return an AST representation of `source`
    */
  def parseInline(source: String): AstExpression = {
    val parsed = new EnsoParser().parseEnsoInline(source)
    parsed
  }

  /**
    * Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST input
    * @return an IR representation with a 1:1 mapping to the parser AST
    *         constructs
    */
  def translate(sourceAST: AST): AstModuleScope =
    AstToAstExpression.translate(sourceAST)
}

package org.enso.compiler

import java.io.StringReader

import com.oracle.truffle.api.TruffleFile
import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.IR.Expression
import org.enso.compiler.core.IR.Module
import org.enso.compiler.generate.AstToAstExpression
import org.enso.flexer.Reader
import org.enso.interpreter.builder.{ExpressionFactory, ModuleScopeExpressionFactory}
import org.enso.interpreter.node.ExpressionNode
import org.enso.interpreter.runtime.error.ModuleDoesNotExistException
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope, TopLevelScope}
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.Language
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
    val parsedAST = parse(source)
    val expr      = translate(parsedAST)
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
    * @param language current language instance
    * @param localScope local scope to process the source in
    * @param moduleScope module scope to process the source in
    * @return an expression node representing the parsed and analyzed source
    */
  def runInline(
    srcString: String,
    language: Language,
    localScope: LocalScope,
    moduleScope: ModuleScope
  ): Option[ExpressionNode] = {
    val source = Source
      .newBuilder(
        LanguageInfo.ID,
        new StringReader(srcString),
        "<interactive_source>"
      )
      .build()
    val parsed: AST = parse(source)

    translateInline(parsed).flatMap { ast =>
      Some(
        new ExpressionFactory(
          language,
          source,
          localScope,
          "<inline_source>",
          moduleScope
        ).run(ast)
      )
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
  def requestProcess(qualifiedName: String): ModuleScope = {
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
  def parse(source: Source): AST = {
    val parser: Parser = Parser()
    val unresolvedAST: AST.Module =
      parser.run(new Reader(source.getCharacters.toString))
    val resolvedAST: AST.Module = parser.dropMacroMeta(unresolvedAST)

    resolvedAST
  }

  /**
    * Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST input
    * @return an IR representation of the program represented by `sourceAST`
    */
  def translate(sourceAST: AST): Module =
    AstToAstExpression.translate(sourceAST)

  /**
    * Lowers the input AST to the compiler's high-level intermediate
    * representation.
    *
    * @param sourceAST the parser AST representing the program source
    * @return an IR representation of the program represented by `sourceAST`
    */
  def translateInline(sourceAST: AST): Option[Expression] =
    AstToAstExpression.translateInline(sourceAST)
}

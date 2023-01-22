package org.enso.compiler.test

import org.enso.compiler.EnsoCompiler
import org.enso.compiler.codegen.AstToIr
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.{PassConfiguration, PassManager}
import org.enso.syntax.text.{AST, Parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.enso.interpreter.runtime.Module
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.pkg.QualifiedName

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
trait CompilerRunner {

  def useRustParser: Boolean = false

  // === IR Utilities =========================================================

  /** Adds an extension method for converting a string to its AST
    * representation.
    *
    * @param source the source code to convert
    */
  implicit class ToAST(source: String) {

    /** Produces the [[AST]] representation of [[source]].
      *
      * @return [[source]] as an AST
      */
    def toAst: AST = {
      val parser: Parser = Parser()
      val unresolvedAST  = parser.runWithIds(source)

      parser.dropMacroMeta(unresolvedAST)
    }

    /** Produces the [[AST]] representation of [[source]] without dropping the
      * macro metadata.
      *
      * @return [[source]] as an AST
      */
    def toAstWithMeta: AST = {
      val parser: Parser = Parser()
      parser.runWithIds(source)
    }
  }

  /** An extension method to allow converting string source code to IR as a
    * module.
    *
    * @param source the source code to convert
    */
  implicit class ToIrModule(source: String) {

    /** Converts program text to a top-level Enso module.
      *
      * @return the [[IR]] representing [[source]]
      */
    def toIrModule: IR.Module = {
      if (useRustParser) {
        val compiler = new EnsoCompiler()
        try compiler.compile(source)
        finally compiler.close()
      } else {
        AstToIr.translate(source.toAst)
      }
    }
  }

  /** An extension method to allow converting string source code to IR as an
    * expression.
    *
    * @param source the source code to convert
    */
  implicit class ToIrExpression(source: String) {

    /** Converts the program text to an Enso expression.
      *
      * @return the [[IR]] representing [[source]], if it is a valid expression
      */
    def toIrExpression: Option[IR.Expression] = {
      AstToIr.translateInline(source.toAst)
    }
  }

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnModule(ir: IR.Module) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param moduleContext the module context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      moduleContext: ModuleContext
    ): IR.Module = {
      passManager.runPassesOnModule(ir, moduleContext)
    }
  }

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnExpression(ir: IR.Expression) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param inlineContext the inline context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      inlineContext: InlineContext
    ): IR.Expression = {
      passManager.runPassesInline(ir, inlineContext)
    }
  }

  /** Adds an extension method to preprocess the source as IR.
    *
    * @param source the source code to preprocess
    */
  implicit class Preprocess(source: String)(implicit
    passManager: PassManager
  ) {

    /** Translates the source code into appropriate IR for testing this pass.
      *
      * @return IR appropriate for testing the alias analysis pass as a module
      */
    def preprocessModule(implicit moduleContext: ModuleContext): IR.Module = {
      source.toIrModule.runPasses(passManager, moduleContext)
    }

    /** Translates the source code into appropriate IR for testing this pass
      *
      * @return IR appropriate for testing the alias analysis pass as an
      *         expression
      */
    def preprocessExpression(implicit
      inlineContext: InlineContext
    ): Option[IR.Expression] = {
      source.toIrExpression.map(_.runPasses(passManager, inlineContext))
    }
  }

  /** Generates a random identifier.
    *
    * @return a random identifier
    */
  def genId: IR.Identifier = IR.randomId

  // === IR Testing Utils =====================================================

  /** A variety of extension methods on IR expressions to aid testing.
    *
    * @param ir the expression to add extension methods to
    */
  implicit class ExpressionAs(ir: IR.Expression) {

    /** Hoists the provided expression into the body of a method.
      *
      * @return a method containing `ir` as its body
      */
    def asMethod: IR.Module.Scope.Definition.Method = {
      IR.Module.Scope.Definition.Method.Explicit(
        IR.Name.MethodReference(
          Some(
            IR.Name.Qualified(
              List(
                IR.Name
                  .Literal("TestType", isMethod = false, None)
              ),
              None
            )
          ),
          IR.Name
            .Literal("testMethod", isMethod = false, None),
          None
        ),
        ir,
        None
      )
    }

    /** Hoists the provided expression as the default value of an atom argument.
      *
      * @return an atom with one argument `arg` with default value `ir`
      */
    def asAtomDefaultArg: IR.Module.Scope.Definition.Data = {
      IR.Module.Scope.Definition.Data(
        IR.Name.Literal("TestAtom", isMethod = false, None),
        List(
          IR.DefinitionArgument
            .Specified(
              IR.Name
                .Literal("arg", isMethod = false, None),
              None,
              Some(ir),
              suspended = false,
              None
            )
        ),
        List(),
        None
      )
    }
  }

  /** Builds a module context with a mocked module for testing purposes.
    *
    * @param moduleName the name of the test module.
    * @param freshNameSupply the fresh name supply to use in tests.
    * @param passConfiguration any additional pass configuration.
    * @return an instance of module context.
    */
  def buildModuleContext(
    moduleName: QualifiedName                    = QualifiedName.simpleName("Test_Module"),
    freshNameSupply: Option[FreshNameSupply]     = None,
    passConfiguration: Option[PassConfiguration] = None,
    compilerConfig: CompilerConfig               = defaultConfig,
    isGeneratingDocs: Boolean                    = false
  ): ModuleContext = {
    ModuleContext(
      module            = Module.empty(moduleName, null, null),
      freshNameSupply   = freshNameSupply,
      passConfiguration = passConfiguration,
      compilerConfig    = compilerConfig,
      isGeneratingDocs  = isGeneratingDocs
    )
  }

  /** Builds an inline context with a mocked module for testing purposes.
    *
    * @param localScope the local scope for variable resolution.
    * @param isInTailPosition whether the expression is being evaluated in
    *                         a tail position.
    * @param freshNameSupply the fresh name supply to use for name generation.
    * @param passConfiguration any additional pass configuration.
    * @return an instance of inline context.
    */
  def buildInlineContext(
    localScope: Option[LocalScope]               = None,
    isInTailPosition: Option[Boolean]            = None,
    freshNameSupply: Option[FreshNameSupply]     = None,
    passConfiguration: Option[PassConfiguration] = None,
    compilerConfig: CompilerConfig               = defaultConfig
  ): InlineContext = {
    val mod = Module.empty(QualifiedName.simpleName("Test_Module"), null, null)
    mod.unsafeSetIr(
      IR.Module(List(), List(), List(), None)
        .updateMetadata(
          BindingAnalysis -->> BindingsMap(
            List(),
            ModuleReference.Concrete(mod)
          )
        )
    )
    mod.unsafeSetCompilationStage(Module.CompilationStage.AFTER_CODEGEN)
    InlineContext(
      module            = mod,
      freshNameSupply   = freshNameSupply,
      passConfiguration = passConfiguration,
      localScope        = localScope,
      isInTailPosition  = isInTailPosition,
      compilerConfig    = compilerConfig
    )
  }

  val defaultConfig: CompilerConfig = CompilerConfig()
}

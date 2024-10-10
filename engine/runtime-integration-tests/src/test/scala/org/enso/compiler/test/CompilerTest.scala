package org.enso.compiler.test

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.{EnsoParser, IR, Identifier}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Diagnostic,
  Expression,
  Module,
  Name
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.data.BindingsMap.ModuleReference
import org.enso.compiler.data.{BindingsMap, CompilerConfig}
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.{PassConfiguration, PassManager}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.ModuleTestUtils
import org.enso.compiler.context.LocalScope
import org.enso.pkg.QualifiedName
import org.enso.common.CompilationStage

import java.util.UUID

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
trait CompilerRunner {
  // === IR Utilities =========================================================

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnModule(ir: Module) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param moduleContext the module context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      moduleContext: ModuleContext
    ): Module = {
      passManager.runPassesOnModule(ir, moduleContext)
    }
  }

  /** Provides an extensions to work with diagnostic information attached to IR.
    *
    * @param ir the IR node
    */
  implicit class DiagnosticStorageExt(ir: IR) {

    /** Get the list of diagnostics attached to the provided IR node without
      * unnecessary allocations of [[org.enso.compiler.core.ir.DiagnosticStorage]].
      *
      * @return the list of attached diagnostics
      */
    def diagnosticsList: List[Diagnostic] =
      if (ir.diagnostics() eq null) Nil else ir.diagnostics().toList
  }

  /** Provides an extension method allowing the running of a specified list of
    * passes on the provided IR.
    *
    * @param ir the IR to run the passes on
    */
  implicit class RunPassesOnExpression(ir: Expression) {

    /** Executes the passes using `passManager` on the input [[ir]].
      *
      * @param passManager the pass configuration
      * @param inlineContext the inline context it is executing in
      * @return the result of executing the passes in `passManager` on [[ir]]
      */
    def runPasses(
      passManager: PassManager,
      inlineContext: InlineContext
    ): Expression = {
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
    def preprocessModule(implicit moduleContext: ModuleContext): Module = {
      EnsoParser.compile(source).runPasses(passManager, moduleContext)
    }

    /** Translates the source code into appropriate IR for testing this pass
      *
      * @return IR appropriate for testing the alias analysis pass as an
      *         expression
      */
    def preprocessExpression(implicit
      inlineContext: InlineContext
    ): Option[Expression] = {
      EnsoParser
        .compileInline(source)
        .map(_.runPasses(passManager, inlineContext))
    }
  }

  /** Generates a random identifier.
    *
    * @return a random identifier
    */
  def genId: UUID @Identifier = IR.randomId

  // === IR Testing Utils =====================================================

  /** A variety of extension methods on IR expressions to aid testing.
    *
    * @param ir the expression to add extension methods to
    */
  implicit class ExpressionAs(ir: Expression) {

    /** Hoists the provided expression into the body of a method.
      *
      * @return a method containing `ir` as its body
      */
    def asMethod: definition.Method = {
      new definition.Method.Explicit(
        definition.Method.Binding(
          Name.MethodReference(
            Some(
              Name.Qualified(
                List(
                  Name.Literal(
                    "TestType",
                    isMethod           = false,
                    identifiedLocation = null
                  )
                ),
                identifiedLocation = null
              )
            ),
            Name.Literal(
              "testMethod",
              isMethod           = false,
              identifiedLocation = null
            ),
            identifiedLocation = null
          ),
          Nil,
          false,
          ir,
          identifiedLocation = null
        ),
        ir
      )
    }

    /** Hoists the provided expression as the default value of an atom argument.
      *
      * @return an atom with one argument `arg` with default value `ir`
      */
    def asAtomDefaultArg: Definition.Data = {
      Definition.Data(
        Name.Literal("TestAtom", isMethod = false, identifiedLocation = null),
        List(
          DefinitionArgument
            .Specified(
              Name
                .Literal("arg", isMethod = false, identifiedLocation = null),
              None,
              Some(ir),
              suspended          = false,
              identifiedLocation = null
            )
        ),
        List(),
        false,
        identifiedLocation = null
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
  ): ModuleContext = buildModuleContextModule(
    moduleName,
    freshNameSupply,
    passConfiguration,
    compilerConfig,
    isGeneratingDocs
  )._1

  /** Builds a module context with a mocked module for testing purposes.
    *
    * @param moduleName the name of the test module.
    * @param freshNameSupply the fresh name supply to use in tests.
    * @param passConfiguration any additional pass configuration.
    * @return an pair of module context and module.
    */
  def buildModuleContextModule(
    moduleName: QualifiedName                    = QualifiedName.simpleName("Test_Module"),
    freshNameSupply: Option[FreshNameSupply]     = None,
    passConfiguration: Option[PassConfiguration] = None,
    compilerConfig: CompilerConfig               = defaultConfig,
    isGeneratingDocs: Boolean                    = false
  ): (ModuleContext, runtime.Module) = {
    val mod = runtime.Module.empty(moduleName, null)
    val ctx = ModuleContext(
      module            = mod.asCompilerModule(),
      freshNameSupply   = freshNameSupply,
      passConfiguration = passConfiguration,
      compilerConfig    = compilerConfig,
      isGeneratingDocs  = isGeneratingDocs
    )
    (ctx, mod)
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
    val mod =
      runtime.Module.empty(QualifiedName.simpleName("Test_Module"), null)
    ModuleTestUtils.unsafeSetIr(
      mod,
      Module(List(), List(), List(), false, identifiedLocation = null)
        .updateMetadata(
          new MetadataPair(
            BindingAnalysis,
            BindingsMap(
              List(),
              ModuleReference.Concrete(mod.asCompilerModule())
            )
          )
        )
    )
    ModuleTestUtils.unsafeSetCompilationStage(
      mod,
      CompilationStage.AFTER_CODEGEN
    )
    val mc = ModuleContext(
      module         = mod.asCompilerModule(),
      compilerConfig = compilerConfig
    )
    InlineContext(
      module            = mc,
      freshNameSupply   = freshNameSupply,
      passConfiguration = passConfiguration,
      localScope        = localScope,
      isInTailPosition  = isInTailPosition,
      compilerConfig    = compilerConfig
    )
  }

  val defaultConfig: CompilerConfig = CompilerConfig()
}

package org.enso.compiler.test

import org.enso.compiler.Passes
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.Expression
import org.enso.compiler.core.ir.Module
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  AmbiguousImportsAnalysis,
  BindingAnalysis,
  ImportSymbolAnalysis,
  PrivateModuleAnalysis
}
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.lint.{ModuleNameConflicts, ShadowedPatternFields}
import org.enso.compiler.pass.optimise.UnreachableMatchBranches
import org.enso.compiler.pass.resolve._

class PassesTest extends CompilerTest {

  // === Test Setup ===========================================================

  case object Pass1 extends IRPass {
    override type Metadata = IRPass.Metadata.Empty
    override type Config   = IRPass.Configuration.Default

    override lazy val precursorPasses: Seq[IRPass]   = List()
    override lazy val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: Module,
      moduleContext: ModuleContext
    ): Module = ir

    override def runExpression(
      ir: Expression,
      inlineContext: InlineContext
    ): Expression = ir
  }

  // === The Tests ============================================================

  "Compiler pass ordering slicing" should {
    val passes = new Passes(defaultConfig)

    "get the precursors of a given pass" in {
      passes.getPrecursors(AliasAnalysis).map(_.passes) shouldEqual Some(
        List(
          ModuleAnnotations,
          DocumentationComments,
          Imports,
          ComplexType,
          FunctionBinding,
          GenerateMethodBodies,
          BindingAnalysis,
          ModuleNameConflicts,
          MethodDefinitions,
          SectionsToBinOp,
          OperatorToFunction,
          LambdaShorthandToLambda,
          ImportSymbolAnalysis,
          AmbiguousImportsAnalysis,
          PrivateModuleAnalysis.getInstance(),
          ShadowedPatternFields,
          UnreachableMatchBranches,
          NestedPatternMatch,
          IgnoredBindings,
          TypeFunctions,
          TypeSignatures,
          ExpressionAnnotations
        )
      )
    }

    "return `None` if the pass doesn't exists" in {
      passes.getPrecursors(Pass1) should not be defined
    }
  }
}

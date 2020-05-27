package org.enso.compiler.test

import org.enso.compiler.Passes
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.resolve.IgnoredBindings

class PassesTest extends CompilerTest {

  // === Test Setup ===========================================================

  case object Pass1 extends IRPass {
    override type Metadata = IRPass.Metadata.Empty
    override type Config   = IRPass.Configuration.Default

    override val precursorPasses: Seq[IRPass]   = List()
    override val invalidatedPasses: Seq[IRPass] = List()

    override def runModule(
      ir: IR.Module,
      moduleContext: ModuleContext
    ): IR.Module = ir

    override def runExpression(
      ir: IR.Expression,
      inlineContext: InlineContext
    ): IR.Expression = ir
  }

  // === The Tests ============================================================

  "Compiler pass ordering slicing" should {
    val passes = new Passes

    "get the precursors of a given pass" in {
      passes.getPrecursors(AliasAnalysis) shouldEqual Some(
        List(
          ComplexType,
          FunctionBinding,
          GenerateMethodBodies,
          SectionsToBinOp,
          OperatorToFunction,
          LambdaShorthandToLambda,
          IgnoredBindings
        )
      )
    }

    "return `None` if the pass doesn't exists" in {
      passes.getPrecursors(Pass1) should not be defined
    }
  }
}

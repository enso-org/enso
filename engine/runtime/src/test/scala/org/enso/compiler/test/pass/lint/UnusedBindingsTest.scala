package org.enso.compiler.test.pass.lint

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.desugar.{GenerateMethodBodies, LambdaShorthandToLambda, OperatorToFunction, SectionsToBinOp}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.{ApplicationSaturation, LambdaConsolidate}
import org.enso.compiler.pass.resolve.IgnoredBindings
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope

import scala.annotation.unused

class UnusedBindingsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: List[IRPass] = passes.getPrecursors(UnusedBindings).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    ApplicationSaturation -->> ApplicationSaturation.Configuration(),
    AliasAnalysis         -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(precursorPasses, passConfiguration)

  /** Adds an extension method for running linting on the input IR.
    *
    * @param ir the IR to lint
    */
  implicit class LintExpression(ir: IR.Expression) {

    /** Runs unused name linting on [[ir]].
      *
      * @param inlineContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all unused names linted
      */
    def lint(implicit inlineContext: InlineContext): IR.Expression = {
      UnusedBindings.runExpression(ir, inlineContext)
    }
  }

  /** Makes an inline context.
    *
    * @return a new inline context
    */
  def mkInlineContext: InlineContext = {
    InlineContext(
      freshNameSupply  = Some(new FreshNameSupply),
      localScope       = Some(LocalScope.root),
      isInTailPosition = Some(false)
    )
  }

  // === The Tests ============================================================

  "Unused bindings linting" should {
    "attach a warning to an unused function argument" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x -> 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Function.Lambda]

      val lintMeta = ir.arguments.head.diagnostics.collect {
        case u: IR.Warning.Unused.FunctionArgument => u
      }

      lintMeta should not be empty
      lintMeta.head shouldBe an[IR.Warning.Unused.FunctionArgument]
      lintMeta.head.name.name shouldEqual "x"
    }

    "not attach a warning to an unused function argument if it is an ignore" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |_ -> 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Function.Lambda]

      ir.arguments.head.diagnostics.toList shouldBe empty
    }

    "attach a warning to an unused binding" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |a = 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Expression.Binding]

      val lintMeta = ir.diagnostics.collect {
        case u: IR.Warning.Unused.Binding => u
      }

      lintMeta should not be empty
      lintMeta.head shouldBe an[IR.Warning.Unused.Binding]
      lintMeta.head.name.name shouldEqual "a"
    }

    "not attach a warning to an unused binding if it is an ignore" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |_ = 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Expression.Binding]

      ir.diagnostics.toList shouldBe empty
    }
  }
}

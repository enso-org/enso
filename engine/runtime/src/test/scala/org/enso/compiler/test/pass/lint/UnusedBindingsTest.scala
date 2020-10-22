package org.enso.compiler.test.pass.lint

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Pattern
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.scalatest.Inside

class UnusedBindingsTest extends CompilerTest with Inside {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: PassGroup = passes.getPrecursors(UnusedBindings).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    ApplicationSaturation -->> ApplicationSaturation.Configuration(),
    AliasAnalysis         -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

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
    buildInlineContext(
      localScope       = Some(LocalScope.root),
      isInTailPosition = Some(false),
      freshNameSupply  = Some(new FreshNameSupply)
    )
  }

  /** Adds an extension method for running linting on the input IR.
    *
    * @param ir the IR to lint
    */
  implicit class LintModule(ir: IR.Module) {

    /** Runs unused name linting on [[ir]].
      *
      * @param moduleContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all unused names linted
      */
    def lint(implicit moduleContext: ModuleContext): IR.Module = {
      UnusedBindings.runModule(ir, moduleContext)
    }
  }

  /** Makes a module context.
    *
    * @return a new inline context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
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

    "attach a warning to an unused top-level function argument" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |f = x -> 10
          |main =
          |    f 0
          |""".stripMargin.preprocessModule.lint

      inside(ir.bindings.head) {
        case definition: IR.Module.Scope.Definition.Method.Explicit =>
          inside(definition.body) { case f: IR.Function.Lambda =>
            val lintMeta = f.arguments(1).diagnostics.collect {
              case u: IR.Warning.Unused.FunctionArgument => u
            }

            lintMeta should not be empty
            lintMeta.head shouldBe an[IR.Warning.Unused.FunctionArgument]
            lintMeta.head.name.name shouldEqual "x"
          }
      }
    }

    "not attach a warning to an unused function argument if it is an ignore" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |_ -> 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Function.Lambda]

      val lintMeta = ir.arguments.head.diagnostics.collect {
        case u: IR.Warning.Unused => u
      }

      lintMeta shouldBe empty
    }

    "attach a warning to an unused binding" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |a = 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Expression.Binding]

      val lintMeta = ir.diagnostics.collect { case u: IR.Warning.Unused =>
        u
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

      val lintMeta = ir.diagnostics.collect { case u: IR.Warning.Unused =>
        u
      }

      lintMeta shouldBe empty
    }

    "warn on unused bindings in patterns" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |case x of
          |    Cons a _ -> 10
          |""".stripMargin.preprocessExpression.get.lint
          .asInstanceOf[IR.Expression.Block]
          .returnValue
          .asInstanceOf[IR.Case.Expr]

      val pattern = ir.branches.head.pattern.asInstanceOf[Pattern.Constructor]
      val field1  = pattern.fields.head.asInstanceOf[Pattern.Name]
      val field2  = pattern.fields(1).asInstanceOf[Pattern.Name]

      val lintMeta1 = field1.diagnostics.collect { case u: IR.Warning.Unused =>
        u
      }
      val lintMeta2 = field2.diagnostics.collect { case u: IR.Warning.Unused =>
        u
      }

      lintMeta1 should not be empty
      lintMeta1.head shouldBe an[IR.Warning.Unused.PatternBinding]
      lintMeta1.head.name.name shouldEqual "a"

      lintMeta2 shouldBe empty
    }
  }
}

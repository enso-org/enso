package org.enso.compiler.test.pass.lint

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{Pattern, Warning}
import org.enso.compiler.pass.lint.ShadowedPatternFields
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ShadowedPatternFieldsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: PassGroup =
    passes.getPrecursors(ShadowedPatternFields).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Creates an extension method for linting an expression for shadowed pattern
    * variables.
    *
    * @param ir the expression to lint
    */
  implicit class LintExxpression(ir: IR.Expression) {

    /** Lints [[ir]] for shadowed pattern variables.
      *
      * @param inlineContext the context in which linting is taking place
      * @return [[ir]], with shadowed pattern variables linted
      */
    def lint(implicit inlineContext: InlineContext): IR.Expression = {
      ShadowedPatternFields.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Shadowed pattern fields linting" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |case f of
        |    Foo a b a -> a + a
        |""".stripMargin.preprocessExpression.get.lint
        .asInstanceOf[IR.Case.Expr]

    val pattern = ir.branches.head.pattern.asInstanceOf[Pattern.Constructor]

    "replace shadowed fields with blanks" in {
      ir.branches.head.pattern shouldBe a[Pattern.Constructor]

      pattern.fields.head
        .asInstanceOf[Pattern.Name]
        .name shouldBe an[IR.Name.Blank]
      pattern
        .fields(1)
        .asInstanceOf[Pattern.Name]
        .name shouldBe an[IR.Name.Literal]
    }

    "attach a shadowing warning to each shadowed field" in {
      atLeast(1, pattern.fields.head.diagnostics.toList) shouldBe a[
        Warning.Shadowed.PatternBinding
      ]

      val warning = pattern.fields.head.diagnostics.collect {
        case w: Warning.Shadowed.PatternBinding => w
      }.head

      warning.shadowedName shouldEqual "a"
      warning.shadower shouldEqual pattern.fields(2)
    }
  }
}

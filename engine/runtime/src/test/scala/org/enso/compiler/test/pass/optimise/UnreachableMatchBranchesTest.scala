package org.enso.compiler.test.pass.optimise

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Warning
import org.enso.compiler.pass.optimise.UnreachableMatchBranches
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest

class UnreachableMatchBranchesTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: List[IRPass] =
    passes.getPrecursors(UnreachableMatchBranches).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(precursorPasses, passConfig)

  /** Adds an extension method to optimise an IR by removing unreachable case
    * branches.
    *
    * @param ir the ir to optimise
    */
  implicit class OptimizeExpression(ir: IR.Expression) {

    /** Optimises [[ir]] by removing unreachable case branches.
     *
     * @param inlineContext the context in which optimization is taking place
     * @return [[ir]] with unreachable case branches removed
     */
    def optimize(implicit inlineContext: InlineContext): IR.Expression = {
      UnreachableMatchBranches.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    InlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Unreachable match detection" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """case x of
        |    Some a -> case a of
        |        Something -> 10
        |        a -> 20
        |        _ -> 30
        |    _ -> 100
        |    a -> 30
        |""".stripMargin.preprocessExpression.get.optimize
        .asInstanceOf[IR.Case.Expr]

    "associate a warning with the case expression" in {
      atLeast(1, ir.diagnostics.toList) shouldBe a[Warning.Unreachable.Branches]
    }

    "remove unreachable branches" in {
      ir.branches.length shouldEqual 2
    }

    "work recursively" in {
      val nestedCase = ir.branches.head.expression.asInstanceOf[IR.Case.Expr]
      nestedCase.branches.length shouldEqual 2
    }
  }
}

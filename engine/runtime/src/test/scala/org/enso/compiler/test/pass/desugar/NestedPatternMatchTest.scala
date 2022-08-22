package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Pattern
import org.enso.compiler.pass.desugar.NestedPatternMatch
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class NestedPatternMatchTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(NestedPatternMatch).get
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run nested pattern desugaring on an
    * [[IR.Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: IR.Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any nested patterns desugared
      */
    def desugar(implicit moduleContext: ModuleContext): IR.Module = {
      NestedPatternMatch.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to run nested pattern desugaring on an arbitrary
    * expression.
    *
    * @param ir the expression to desugar
    */
  implicit class DesugarExpression(ir: IR.Expression) {

    /** Runs desgaring on an expression.
      *
      * @param inlineContext the inline context in which the desugaring is
      *                      taking place
      * @return [[ir]], with nested patterns desugared
      */
    def desugar(implicit inlineContext: InlineContext): IR.Expression = {
      NestedPatternMatch.runExpression(ir, inlineContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext(freshNameSupply = Some(new FreshNameSupply))
  }

  /** Creates a defaulted inline context.
    *
    * @return a defaulted inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Nested pattern detection" should {
    implicit val ctx: InlineContext = mkInlineContext

    "work properly on named patterns" in {
      val ir =
        """
          |case x of
          |    a -> a
          |    _ -> 0
          |""".stripMargin.preprocessExpression.get.asInstanceOf[IR.Case.Expr]

      val pattern1 = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern1) shouldEqual false
    }

    "work properly on non-nested patterns" in {
      val ir =
        """
          |case x of
          |    Cons a b -> a + b
          |""".stripMargin.preprocessExpression.get.asInstanceOf[IR.Case.Expr]

      val pattern = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern) shouldEqual false
    }

    "work properly on nested patterns" in {
      val ir =
        """
          |case x of
          |    Cons (Cons a b) c -> a + b
          |""".stripMargin.preprocessExpression.get.asInstanceOf[IR.Case.Expr]

      val pattern = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern) shouldEqual true
    }

    "work properly on constructor patterns" in {
      val ir =
        """case x of
          |    Cons a Nil -> a
          |""".stripMargin.preprocessExpression.get.asInstanceOf[IR.Case.Expr]

      val pattern = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern) shouldEqual true
    }
  }

  "Nested pattern desugaring" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |case x of
        |    Cons (Cons MyAtom b) Nil -> a + b
        |    Cons (Cons 1) Nil -> 1
        |    Cons a Nil -> a
        |    _ -> case y of
        |        Cons a Nil -> a
        |        _ -> 0
        |""".stripMargin.preprocessExpression.get.desugar
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]

    val consConsNilBranch    = ir.branches(0)
    val consConsOneNilBranch = ir.branches(1)
    val consANilBranch       = ir.branches(2)
    val catchAllBranch       = ir.branches(3)

    "desugar nested constructors to simple patterns" in {
      consANilBranch.expression shouldBe an[IR.Expression.Block]
      consANilBranch.pattern shouldBe an[IR.Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consANilBranch.pattern) shouldEqual false

      val nestedCase = consANilBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]

      nestedCase.scrutinee shouldBe an[IR.Name.Literal]
      nestedCase.branches.length shouldEqual 2

      val nilBranch      = nestedCase.branches(0)
      val fallbackBranch = nestedCase.branches(1)

      nilBranch.pattern shouldBe a[Pattern.Constructor]
      nilBranch.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Nil"
      nilBranch.expression shouldBe an[IR.Name.Literal]
      nilBranch.expression.asInstanceOf[IR.Name].name shouldEqual "a"

      fallbackBranch.pattern shouldBe a[Pattern.Name]
      fallbackBranch.pattern
        .asInstanceOf[Pattern.Name]
        .name shouldBe an[IR.Name.Blank]

      fallbackBranch.expression shouldBe an[IR.Expression.Block]
      fallbackBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]
        .branches
        .length shouldEqual 1
    }

    "desugar deeply nested patterns to simple patterns" in {
      consConsNilBranch.expression shouldBe an[IR.Expression.Block]
      consConsNilBranch.pattern shouldBe an[IR.Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consConsNilBranch.pattern) shouldEqual false

      val nestedCase = consConsNilBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]

      nestedCase.scrutinee shouldBe an[IR.Name.Literal]
      nestedCase.branches.length shouldEqual 2

      val consBranch      = nestedCase.branches(0)
      val fallbackBranch1 = nestedCase.branches(1)

      consBranch.expression shouldBe an[IR.Expression.Block]
      fallbackBranch1.expression shouldBe an[IR.Expression.Block]

      val consBranchBody = consBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]
      val fallbackBranch1Body =
        fallbackBranch1.expression
          .asInstanceOf[IR.Expression.Block]
          .returnValue
          .asInstanceOf[IR.Case.Expr]

      consBranchBody.branches.length shouldEqual 2
      consBranchBody.branches.head.expression shouldBe an[IR.Expression.Block]
      consBranchBody.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "MyAtom"
      NestedPatternMatch.containsNestedPatterns(
        consBranchBody.branches.head.pattern
      ) shouldEqual false

      fallbackBranch1Body.branches.length shouldEqual 3
      fallbackBranch1Body.branches.head.pattern shouldBe a[Pattern.Constructor]
      fallbackBranch1Body.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Cons"
      NestedPatternMatch.containsNestedPatterns(
        fallbackBranch1Body.branches.head.pattern
      ) shouldEqual false
    }

    "desugar deeply nested patterns with literals to simple patterns" in {
      consConsOneNilBranch.expression shouldBe an[IR.Expression.Block]
      consConsOneNilBranch.pattern shouldBe an[IR.Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consConsOneNilBranch.pattern) shouldEqual false

      val nestedCase = consConsOneNilBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]

      nestedCase.scrutinee shouldBe an[IR.Name.Literal]
      nestedCase.branches.length shouldEqual 2

      val consBranch      = nestedCase.branches(0)
      val fallbackBranch1 = nestedCase.branches(1)

      consBranch.expression shouldBe an[IR.Expression.Block]
      fallbackBranch1.expression shouldBe an[IR.Expression.Block]

      val consBranchBody = consBranch.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]
      val fallbackBranch1Body =
        fallbackBranch1.expression
          .asInstanceOf[IR.Expression.Block]
          .returnValue
          .asInstanceOf[IR.Case.Expr]

      consBranchBody.branches.length shouldEqual 2
      consBranchBody.branches.head.expression shouldBe an[IR.Expression.Block]
      consBranchBody.branches.head.pattern
        .asInstanceOf[Pattern.Literal]
        .literal
        .asInstanceOf[IR.Literal.Number]
        .numericValue shouldEqual 1
      NestedPatternMatch.containsNestedPatterns(
        consBranchBody.branches.head.pattern
      ) shouldEqual false

      fallbackBranch1Body.branches.length shouldEqual 2
      fallbackBranch1Body.branches.head.pattern shouldBe a[Pattern.Constructor]
      fallbackBranch1Body.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Cons"
      NestedPatternMatch.containsNestedPatterns(
        fallbackBranch1Body.branches.head.pattern
      ) shouldEqual false
    }

    "work recursively" in {
      catchAllBranch.expression shouldBe an[IR.Expression.Block]
      val consANilBranch2 =
        catchAllBranch.expression
          .asInstanceOf[IR.Expression.Block]
          .returnValue
          .asInstanceOf[IR.Case.Expr]
          .branches
          .head

      NestedPatternMatch.containsNestedPatterns(
        consANilBranch2.pattern
      ) shouldEqual false
      consANilBranch2.expression shouldBe an[IR.Expression.Block]
      val consANilBranch2Expr =
        consANilBranch2.expression
          .asInstanceOf[IR.Expression.Block]
          .returnValue
          .asInstanceOf[IR.Case.Expr]

      consANilBranch2Expr.branches.length shouldEqual 2
      consANilBranch2Expr.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Nil"
    }
  }
}

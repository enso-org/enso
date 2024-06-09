package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.expression.Case
import org.enso.compiler.core.ir.{Expression, Literal, Module, Name, Pattern}
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
    * [[Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any nested patterns desugared
      */
    def desugar(implicit moduleContext: ModuleContext): Module = {
      NestedPatternMatch.runModule(ir, moduleContext)
    }
  }

  /** Adds an extension method to run nested pattern desugaring on an arbitrary
    * expression.
    *
    * @param ir the expression to desugar
    */
  implicit class DesugarExpression(ir: Expression) {

    /** Runs desgaring on an expression.
      *
      * @param inlineContext the inline context in which the desugaring is
      *                      taking place
      * @return [[ir]], with nested patterns desugared
      */
    def desugar(implicit inlineContext: InlineContext): Expression = {
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
          |""".stripMargin.preprocessExpression.get.asInstanceOf[Case.Expr]

      val pattern1 = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern1) shouldEqual false
    }

    "work properly on non-nested patterns" in {
      val ir =
        """
          |case x of
          |    Cons a b -> a + b
          |""".stripMargin.preprocessExpression.get.asInstanceOf[Case.Expr]

      val pattern = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern) shouldEqual false
    }

    "work properly on nested patterns" in {
      val ir =
        """
          |case x of
          |    Cons (Cons a b) c -> a + b
          |""".stripMargin.preprocessExpression.get.asInstanceOf[Case.Expr]

      val pattern = ir.branches.head.pattern

      NestedPatternMatch.containsNestedPatterns(pattern) shouldEqual true
    }

    "work properly on constructor patterns" in {
      val ir =
        """case x of
          |    Cons a Nil -> a
          |""".stripMargin.preprocessExpression.get.asInstanceOf[Case.Expr]

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
        |    Cons (Cons 1 Nil) Nil -> num
        |    Cons (Cons (num : Integer) _) Nil -> num
        |    Cons a Nil -> a
        |    _ -> case y of
        |        Cons a Nil -> a
        |        _ -> 0
        |""".stripMargin.preprocessExpression.get.desugar
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

    val consConsNilBranch        = ir.branches(0)
    val consConsOneNilBranch     = ir.branches(1)
    val consConsIntegerNilBranch = ir.branches(2)
    val consANilBranch           = ir.branches(3)
    val catchAllBranch           = ir.branches(4)

    "desugar nested constructors to simple patterns" in {
      ir.isNested shouldBe false

      consANilBranch.expression shouldBe an[Expression.Block]
      consANilBranch.pattern shouldBe an[Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consANilBranch.pattern) shouldEqual false
      consANilBranch.terminalBranch shouldBe false

      val nestedCase = consANilBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      nestedCase.scrutinee shouldBe an[Name.Literal]
      nestedCase.branches.length shouldEqual 1
      nestedCase.isNested shouldBe true

      val nilBranch = nestedCase.branches(0)

      nilBranch.pattern shouldBe a[Pattern.Constructor]
      nilBranch.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Nil"
      nilBranch.expression shouldBe an[Name.Literal]
      nilBranch.expression.asInstanceOf[Name].name shouldEqual "a"
      nilBranch.terminalBranch shouldBe true
    }

    "desugar deeply nested patterns to simple patterns" in {
      consConsNilBranch.expression shouldBe an[Expression.Block]
      consConsNilBranch.pattern shouldBe an[Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consConsNilBranch.pattern) shouldEqual false
      consConsNilBranch.terminalBranch shouldBe false

      val nestedCase = consConsNilBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      nestedCase.scrutinee shouldBe an[Name.Literal]
      nestedCase.branches.length shouldEqual 1
      nestedCase.isNested shouldBe true

      val consBranch = nestedCase.branches(0)

      consBranch.expression shouldBe an[Expression.Block]

      val consBranchBody = consBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      consBranchBody.branches.length shouldEqual 1
      consBranchBody.branches.head.expression shouldBe an[Expression.Block]
      consBranchBody.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "MyAtom"
      NestedPatternMatch.containsNestedPatterns(
        consBranchBody.branches.head.pattern
      ) shouldEqual false
    }

    "desugar deeply nested patterns with literals to simple patterns" in {
      consConsOneNilBranch.expression shouldBe an[Expression.Block]
      consConsOneNilBranch.pattern shouldBe an[Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(consConsOneNilBranch.pattern) shouldEqual false
      consConsOneNilBranch.terminalBranch shouldBe false

      val nestedCase = consConsOneNilBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      nestedCase.scrutinee shouldBe an[Name.Literal]
      nestedCase.branches.length shouldEqual 1
      nestedCase.isNested shouldBe true

      val consBranch = nestedCase.branches(0)

      consBranch.expression shouldBe an[Expression.Block]
      consBranch.terminalBranch shouldBe false

      val consBranchBody = consBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      consBranchBody.branches.length shouldEqual 1
      consBranchBody.branches.head.expression shouldBe an[Expression.Block]
      consBranchBody.branches.head.pattern
        .asInstanceOf[Pattern.Literal]
        .literal
        .asInstanceOf[Literal.Number]
        .numericValue shouldEqual 1
      NestedPatternMatch.containsNestedPatterns(
        consBranchBody.branches.head.pattern
      ) shouldEqual false
      consBranchBody.isNested shouldBe true
      consBranchBody.branches.head.terminalBranch shouldBe true
    }

    "desugar deeply nested patterns with type pattern to simple patterns" in {
      consConsIntegerNilBranch.expression shouldBe an[Expression.Block]
      consConsIntegerNilBranch.pattern shouldBe an[Pattern.Constructor]
      NestedPatternMatch
        .containsNestedPatterns(
          consConsIntegerNilBranch.pattern
        ) shouldEqual false
      consConsIntegerNilBranch.terminalBranch shouldBe false

      val nestedCase = consConsIntegerNilBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      nestedCase.scrutinee shouldBe an[Name.Literal]
      nestedCase.branches.length shouldEqual 1
      nestedCase.isNested shouldBe true

      val consBranch = nestedCase.branches(0)

      consBranch.expression shouldBe an[Expression.Block]
      consBranch.terminalBranch shouldBe false

      val consBranchBody = consBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      consBranchBody.branches.length shouldEqual 1
      consBranchBody.branches.head.expression shouldBe an[Expression.Block]
      val tpePattern = consBranchBody.branches.head.pattern
        .asInstanceOf[Pattern.Type]
      consBranchBody.branches.head.terminalBranch shouldBe true

      tpePattern.name
        .asInstanceOf[Name.Literal]
        .name shouldEqual "num"
      tpePattern.tpe.name shouldEqual "Integer"

      NestedPatternMatch.containsNestedPatterns(
        consBranchBody.branches.head.pattern
      ) shouldEqual false

      val consTpeBranchBody = consBranchBody.branches.head.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]
      consTpeBranchBody.branches.length shouldEqual 1

      consTpeBranchBody.branches.head.pattern shouldBe an[Pattern.Constructor]
    }

    "work recursively" in {
      catchAllBranch.expression shouldBe an[Expression.Block]
      catchAllBranch.terminalBranch shouldBe true
      val consANilCase = catchAllBranch.expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      consANilCase.isNested shouldBe false

      val consANilBranch2 =
        consANilCase.branches.head

      NestedPatternMatch.containsNestedPatterns(
        consANilBranch2.pattern
      ) shouldEqual false
      consANilBranch2.terminalBranch shouldBe false
      consANilBranch2.expression shouldBe an[Expression.Block]
      val consANilBranch2Expr =
        consANilBranch2.expression
          .asInstanceOf[Expression.Block]
          .returnValue
          .asInstanceOf[Case.Expr]

      consANilBranch2Expr.isNested shouldBe true
      consANilBranch2Expr.branches.length shouldEqual 1
      consANilBranch2Expr.branches.head.pattern
        .asInstanceOf[Pattern.Constructor]
        .constructor
        .name shouldEqual "Nil"
      consANilBranch2Expr.branches.head.terminalBranch shouldBe true
    }
  }
}

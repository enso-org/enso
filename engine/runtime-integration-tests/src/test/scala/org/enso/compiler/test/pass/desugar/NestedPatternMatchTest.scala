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

  "Simple nested pattern desugaring" should {
    implicit val ctx: InlineContext = mkInlineContext

    // IGV graph: https://github.com/user-attachments/assets/b5387e61-e577-4b03-8a4a-ca05e27f2462
    "One nested pattern" in {
      val ir =
        """
          |case x of
          |    Cons (Nested a) -> num
          |""".stripMargin.preprocessExpression.get
      val processed = ir.desugar

      val block7   = processed.asInstanceOf[Expression.Block]
      val binding8 = block7.expressions.head.asInstanceOf[Expression.Binding]
      binding8.name.name shouldBe "<internal-0>"
      val literal1 = binding8.expression.asInstanceOf[Name.Literal]
      literal1.name shouldBe "x"
      val caseExpr0 = block7.returnValue.asInstanceOf[Case.Expr]
      val literal9  = caseExpr0.scrutinee.asInstanceOf[Name.Literal]
      literal9.name shouldBe "<internal-0>"
      val caseBranch10 = caseExpr0.branches.head
      caseBranch10.terminalBranch shouldBe false
      val patternCons11 = caseBranch10.pattern.asInstanceOf[Pattern.Constructor]
      patternCons11.constructor.name shouldBe "Cons"
      val patternName12 = patternCons11.fields.head.asInstanceOf[Pattern.Name]
      patternName12.name.name shouldBe "<internal-1>"

      val block13   = caseBranch10.expression.asInstanceOf[Expression.Block]
      val binding14 = block13.expressions.head.asInstanceOf[Expression.Binding]
      binding14.name.name shouldBe "<internal-2>"
      val literal15 = binding14.expression.asInstanceOf[Name.Literal]
      literal15.name shouldBe "<internal-1>"
      val caseExpr16 = block13.returnValue.asInstanceOf[Case.Expr]
      val literal17  = caseExpr16.scrutinee.asInstanceOf[Name.Literal]
      literal17.name shouldBe "<internal-2>"
      val caseBranch18 = caseExpr16.branches.head
      caseBranch18.terminalBranch shouldBe true
      val patternCons19 = caseBranch18.pattern.asInstanceOf[Pattern.Constructor]
      patternCons19.constructor.name shouldBe "Nested"
      val patternName20 = patternCons19.fields.head.asInstanceOf[Pattern.Name]
      patternName20.name.name shouldBe "a"
      val literal21 = caseBranch18.expression.asInstanceOf[Name.Literal]
      literal21.name shouldBe "num"
    }
  }
}

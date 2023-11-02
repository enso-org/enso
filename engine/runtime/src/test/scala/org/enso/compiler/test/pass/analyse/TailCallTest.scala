package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  Expression,
  Function,
  Module,
  Pattern,
  Warning
}
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.Case
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.TailCall.TailPosition
import org.enso.compiler.pass.analyse.{AliasAnalysis, TailCall}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.compiler.context.LocalScope

class TailCallTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  def mkTailContext: InlineContext =
    buildInlineContext(
      localScope       = Some(LocalScope.root),
      isInTailPosition = Some(true),
      freshNameSupply  = Some(new FreshNameSupply)
    )

  def mkNoTailContext: InlineContext =
    buildInlineContext(
      localScope       = Some(LocalScope.root),
      isInTailPosition = Some(false),
      freshNameSupply  = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(TailCall).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext) = {
      TailCall.runModule(ir, context)
    }
  }

  /** Adds an extension method to preprocess source code as an Enso expression.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseExpresion(ir: Expression) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the inline context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: InlineContext): Expression = {
      TailCall.runExpression(ir, context)
    }
  }

  // === The Tests ============================================================

  "Tail call analysis on modules" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |
        |    case c of
        |      Baz a b -> a * b * d
        |      _ -> d
        |
        |type MyAtom a b c
        |
        |Foo.from (that : Bar) = undefined
        |""".stripMargin.preprocessModule.analyse

    "mark methods as tail" in {
      ir.bindings.head.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "mark atoms as tail" in {
      ir.bindings(1).getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "mark conversions as tail" in {
      ir.bindings(2).getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }
  }

  "Tail call analysis on expressions" should {
    val code =
      """
        |x -> y -> z -> x y z
        |""".stripMargin

    "mark the expression as tail if the context requires it" in {
      implicit val ctx: InlineContext = mkTailContext
      val ir                          = code.preprocessExpression.get.analyse

      ir.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "not mark the expression as tail if the context doesn't require it" in {
      implicit val ctx: InlineContext = mkNoTailContext
      val ir                          = code.preprocessExpression.get.analyse

      ir.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
    }

    "mark the value of a tail assignment as non-tail" in {
      implicit val ctx: InlineContext = mkTailContext
      val binding =
        """
          |foo = a b
          |""".stripMargin.preprocessExpression.get.analyse
          .asInstanceOf[Expression.Binding]
      binding.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
      binding.expression.getMetadata(TailCall) shouldEqual Some(
        TailPosition.NotTail
      )

    }
  }

  "Tail call analysis on functions" should {
    implicit val ctx: InlineContext = mkTailContext

    val ir =
      """
        |a -> b -> c ->
        |    d = @Tail_Call (a + b)
        |    e = a * c
        |    @Tail_Call (d + e)
        |""".stripMargin.preprocessExpression.get.analyse
        .asInstanceOf[Function.Lambda]

    val fnBody = ir.body.asInstanceOf[Expression.Block]

    "mark the last expression of the function as tail" in {
      fnBody.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the other expressions in the function as not tail" in {
      fnBody.expressions.foreach(expr =>
        expr.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      )
    }

    "warn about misplaced @TailCall annotations" in {
      fnBody
        .expressions(0)
        .asInstanceOf[Expression.Binding]
        .expression
        .diagnostics
        .filter(_.isInstanceOf[Warning.WrongTco])
        .toList
        .length shouldEqual 1

      fnBody.returnValue.diagnostics
        .filter(_.isInstanceOf[Warning.WrongTco])
        .toList
        .length shouldEqual 0
    }
  }

  "Tail call analysis on local functions" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |adder_two =
        |    if 0 == 0 then 0 else
        |        @Tail_Call adder_two
        |""".stripMargin.preprocessModule.analyse

    val fnBody = ir.bindings.head
      .asInstanceOf[definition.Method]
      .body
      .asInstanceOf[Function.Lambda]
      .body

    "handle application involving local functions" in {
      fnBody
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Application.Prefix]
        .arguments(2)
        .value
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Application.Prefix]
        .function
        .diagnostics
        .filter(_.isInstanceOf[Warning.WrongTco])
        .toList
        .length shouldEqual 0
    }

  }

  "Tail call analysis on case expressions" should {
    "not mark any portion of the branch functions as tail by default" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |Foo.bar = a ->
          |    x = case a of
          |        Lambda fn arg -> fn arg
          |
          |    x
          |""".stripMargin.preprocessModule.analyse

      val caseExpr = ir.bindings.head
        .asInstanceOf[definition.Method]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .expressions
        .head
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      caseExpr.getMetadata(TailCall) shouldEqual Some(
        TailPosition.NotTail
      )
      caseExpr.branches.foreach(branch => {
        val branchExpression =
          branch.expression.asInstanceOf[Application.Prefix]

        branchExpression.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      })
    }

    "only mark the branches as tail if the expression is in tail position" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |Foo.bar = a ->
          |    case a of
          |      Lambda fn arg -> fn arg
          |""".stripMargin.preprocessModule.analyse

      val caseExpr = ir.bindings.head
        .asInstanceOf[definition.Method]
        .body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Case.Expr]

      caseExpr.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
      caseExpr.branches.foreach(branch => {
        val branchExpression =
          branch.expression.asInstanceOf[Application.Prefix]

        branchExpression.getMetadata(TailCall) shouldEqual Some(
          TailPosition.Tail
        )
      })
    }

    "mark patters and pattern elements as not tail" in {
      implicit val ctx: InlineContext = mkTailContext

      val ir =
        """
          |case x of
          |    Cons a b -> a + b
          |""".stripMargin.preprocessExpression.get.analyse
          .asInstanceOf[Expression.Block]
          .returnValue
          .asInstanceOf[Case.Expr]

      val caseBranch         = ir.branches.head
      val pattern            = caseBranch.pattern.asInstanceOf[Pattern.Constructor]
      val patternConstructor = pattern.constructor

      pattern.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
      patternConstructor.getMetadata(TailCall) shouldEqual Some(
        TailPosition.NotTail
      )
      pattern.fields.foreach(f => {
        f.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)

        f.asInstanceOf[Pattern.Name]
          .name
          .getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
      })
    }
  }

  "Tail call analysis on function calls" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val tailCall =
      """
        |Foo.bar =
        |   IO.println "AAAAA"
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]
    val tailCallBody = tailCall.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    val nonTailCall =
      """
        |Foo.bar =
        |    a = b c d
        |    a
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]
    val nonTailCallBody = nonTailCall.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    "mark the arguments as tail" in {
      nonTailCallBody.expressions.head
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Application.Prefix]
        .arguments
        .foreach(arg =>
          arg.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        )

      tailCallBody.returnValue
        .asInstanceOf[Application.Prefix]
        .arguments
        .foreach(arg =>
          arg.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        )
    }

    "mark the function call as tail if it is in a tail position" in {
      tailCallBody.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the function call as not tail if it is in a tail position" in {
      nonTailCallBody.expressions.head
        .asInstanceOf[Expression.Binding]
        .expression
        .getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
    }
  }

  "Tail call analysis on blocks" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |    mul = a -> b -> a * b
        |    mul c d
        |""".stripMargin.preprocessModule.analyse.bindings.head
        .asInstanceOf[definition.Method]

    val block = ir.body
      .asInstanceOf[Function.Lambda]
      .body
      .asInstanceOf[Expression.Block]

    "mark the bodies of bound functions as tail properly" in {
      block
        .expressions(1)
        .asInstanceOf[Expression.Binding]
        .expression
        .asInstanceOf[Function.Lambda]
        .body
        .getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "mark the block expressions as not tail" in {
      block.expressions.foreach(expr =>
        expr.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      )
    }

    "mark the final expression of the block as tail" in {
      block.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the block as tail if it is in a tail position" in {
      block.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }
  }
}

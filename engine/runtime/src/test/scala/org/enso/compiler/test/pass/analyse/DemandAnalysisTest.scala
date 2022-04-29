package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.{AliasAnalysis, DemandAnalysis}
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope

class DemandAnalysisTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  /** The passes that must be run before the demand analysis pass. */
  val precursorPasses: PassGroup = passes.getPrecursors(DemandAnalysis).get

  val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run alias analysis on an [[IR.Module]].
    *
    * @param ir the module to run alias analysis on
    */
  implicit class AnalyseModule(ir: IR.Module) {

    /** Runs demand analysis on a module.
      *
      * @return [[ir]], transformed by the demand analysis pass
      */
    def analyse: IR.Module = {
      DemandAnalysis.runModule(ir, buildModuleContext())
    }
  }

  /** Adds an extension method to run alias analysis on an [[IR.Expression]].
    *
    * @param ir the expression to run alias analysis on
    */
  implicit class AnalyseExpression(ir: IR.Expression) {

    /** Runs demand analysis on an expression.
      *
      * @param inlineContext the inline context in which to process the
      *                      expression
      * @return [[ir]], transformed by the demand analysis pass
      */
    def analyse(implicit inlineContext: InlineContext): IR.Expression = {
      DemandAnalysis.runExpression(ir, inlineContext)
    }
  }

  /** Makes an inline context.
    *
    * @return a new inline context
    */
  def mkInlineContext: InlineContext = {
    buildInlineContext(
      localScope      = Some(LocalScope.root),
      freshNameSupply = Some(new FreshNameSupply)
    )
  }

  def mkModuleContext: ModuleContext = {
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )
  }

  // === The Tests ============================================================

  "Suspended arguments" should {
    "be forced when assigned" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> ~y -> z ->
          |    a = x
          |    z
          |""".stripMargin.preprocessExpression.get.analyse

      val boundX = ir
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .expressions
        .head
        .asInstanceOf[IR.Expression.Binding]
        .expression

      boundX shouldBe an[IR.Application.Force]
      boundX.asInstanceOf[IR.Application.Force].target shouldBe an[IR.Name]
    }

    "work correctly when deeply nested" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> b -> a -> x
          |""".stripMargin.preprocessExpression.get.analyse

      val xUsage = ir
        .asInstanceOf[IR.Function.Lambda]
        .body

      xUsage shouldBe an[IR.Application.Force]
      xUsage.asInstanceOf[IR.Application.Force].target shouldBe an[IR.Name]
    }

    "not be forced when passed to functions" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> ~y -> z -> foo x y z
          |""".stripMargin.preprocessExpression.get.analyse

      val app = ir
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Application.Prefix]

      app.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name]

      app
        .arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name]
    }

    "be forced when used in vector literals" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> ~y -> z -> [x, y, z]
          |""".stripMargin.preprocessExpression.get.analyse

      val vec = ir
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Application.Prefix]
        .arguments(0)
        .value
        .asInstanceOf[IR.Application.Literal.Sequence]

      vec.items(0) shouldBe an[IR.Application.Force]
      vec.items(1) shouldBe an[IR.Application.Force]
      vec.items(2) shouldBe an[IR.Application.Force]

    }
  }

  "Suspended blocks" should {
    "be forced when used" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x ->
          |    blck =
          |        foo a b
          |    test = blck
          |    blck
          |""".stripMargin.preprocessExpression.get.analyse

      val irBody = ir
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]

      irBody
        .expressions(1)
        .asInstanceOf[IR.Expression.Binding]
        .expression shouldBe an[IR.Application.Force]

      irBody.returnValue shouldBe an[IR.Application.Force]
    }

    "not be forced when passed to a function" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x ->
          |    blck =
          |        foo a b
          |    bar blck
          |""".stripMargin.preprocessExpression.get.analyse

      ir.asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Application.Prefix]
        .arguments
        .head
        .asInstanceOf[IR.CallArgument.Specified]
        .value shouldBe an[IR.Name]
    }

    "force terms in blocks passed directly as arguments" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |bar ~x = foo <|
          |    x
          |""".stripMargin.preprocessModule.analyse

      val barFunc = ir.bindings.head
        .asInstanceOf[IR.Module.Scope.Definition.Method.Explicit]
      val oprCall = barFunc.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Application.Prefix]

      oprCall.function.asInstanceOf[IR.Name].name shouldEqual "<|"
      oprCall.arguments.length shouldEqual 2

      val xArg = oprCall.arguments(1).asInstanceOf[IR.CallArgument.Specified]

      xArg.value shouldBe an[IR.Expression.Block]
      xArg.value
        .asInstanceOf[IR.Expression.Block]
        .returnValue shouldBe an[IR.Application.Force]
    }
  }
}

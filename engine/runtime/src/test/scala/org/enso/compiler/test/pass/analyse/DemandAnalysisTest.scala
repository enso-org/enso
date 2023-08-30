package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.module.scope.definition
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

  /** Adds an extension method to run alias analysis on an [[Module]].
    *
    * @param ir the module to run alias analysis on
    */
  implicit class AnalyseModule(ir: Module) {

    /** Runs demand analysis on a module.
      *
      * @return [[ir]], transformed by the demand analysis pass
      */
    def analyse: Module = {
      DemandAnalysis.runModule(ir, buildModuleContext())
    }
  }

  /** Adds an extension method to run alias analysis on an [[Expression]].
    *
    * @param ir the expression to run alias analysis on
    */
  implicit class AnalyseExpression(ir: Expression) {

    /** Runs demand analysis on an expression.
      *
      * @param inlineContext the inline context in which to process the
      *                      expression
      * @return [[ir]], transformed by the demand analysis pass
      */
    def analyse(implicit inlineContext: InlineContext): Expression = {
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
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .expressions
        .head
        .asInstanceOf[Expression.Binding]
        .expression

      boundX shouldBe an[Application.Force]
      boundX.asInstanceOf[Application.Force].target shouldBe an[Name]
    }

    "work correctly when deeply nested" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> b -> a -> x
          |""".stripMargin.preprocessExpression.get.analyse

      val xUsage = ir
        .asInstanceOf[Function.Lambda]
        .body

      xUsage shouldBe an[Application.Force]
      xUsage.asInstanceOf[Application.Force].target shouldBe an[Name]
    }

    "not be forced when passed to functions" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> ~y -> z -> foo x y z
          |""".stripMargin.preprocessExpression.get.analyse

      val app = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]

      app.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value shouldBe an[Name]

      app
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value shouldBe an[Name]
    }

    "be forced when used in vector literals" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |~x -> ~y -> z -> [x, y, z]
          |""".stripMargin.preprocessExpression.get.analyse

      val vec = ir
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Sequence]

      vec.items(0) shouldBe an[Application.Force]
      vec.items(1) shouldBe an[Application.Force]
      vec.items(2) shouldBe an[Application.Force]

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
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]

      irBody
        .expressions(1)
        .asInstanceOf[Expression.Binding]
        .expression shouldBe an[Application.Force]

      irBody.returnValue shouldBe an[Application.Force]
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

      ir.asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Expression.Block]
        .returnValue
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value shouldBe an[Name]
    }

    "force terms in blocks passed directly as arguments" in {
      implicit val ctx: ModuleContext = mkModuleContext

      val ir =
        """
          |bar ~x = foo <|
          |    x
          |""".stripMargin.preprocessModule.analyse

      val barFunc = ir.bindings.head
        .asInstanceOf[definition.Method.Explicit]
      val oprCall = barFunc.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]

      oprCall.function.asInstanceOf[Name].name shouldEqual "<|"
      oprCall.arguments.length shouldEqual 2

      val xArg = oprCall.arguments(1).asInstanceOf[CallArgument.Specified]

      xArg.value shouldBe an[Expression.Block]
      xArg.value
        .asInstanceOf[Expression.Block]
        .returnValue shouldBe an[Application.Force]
    }
  }
}

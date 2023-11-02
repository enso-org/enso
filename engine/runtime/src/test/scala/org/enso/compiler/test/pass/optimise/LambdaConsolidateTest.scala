package org.enso.compiler.test.pass.optimise

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.expression.warnings
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.compiler.context.LocalScope

class LambdaConsolidateTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup =
    passes.getPrecursors(LambdaConsolidate).get

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to run lambda consolidation on an [[Module]].
    *
    * @param ir the module to run lambda consolidation on
    */
  implicit class OptimiseModule(ir: Module) {

    /** Runs lambda consolidation on a module.
      *
      * @return [[ir]], with chained lambdas consolidated
      */
    def optimise: Module = {
      LambdaConsolidate.runModule(
        ir,
        buildModuleContext(passConfiguration = Some(passConfiguration))
      )
    }
  }

  /** Adds an extension method to run lambda consolidation on an
    * [[Expression]].
    *
    * @param ir the expression to run lambda consolidation on
    */
  implicit class OptimiseExpression(ir: Expression) {

    /** Runs lambda consolidation on an expression.
      *
      * @param inlineContext the inline context in which to process the
      *                      expression
      * @return [[ir]], with chained lambdas consolidated
      */
    def optimise(implicit inlineContext: InlineContext): Expression = {
      LambdaConsolidate.runExpression(ir, inlineContext)
    }
  }

  /** Makes a default inline context for testing with
    *
    * @return a default inline context
    */
  def mkContext: InlineContext = {
    buildInlineContext(
      localScope        = Some(LocalScope.root),
      freshNameSupply   = Some(new FreshNameSupply),
      passConfiguration = Some(passConfiguration)
    )
  }

  // === The Tests ============================================================

  "Lambda consolidation" should {

    "collapse chained lambdas into a single lambda" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x -> y -> z -> x + y + z
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      ir.arguments.length shouldEqual 3
      ir.body shouldBe an[Application]
    }

    "rename shadowed parameters" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x -> z -> y -> x -> y -> x + y
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      ir.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name should not equal "x"
      ir.body
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name]
        .name shouldEqual "x"
    }

    "work properly with default arguments" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x -> y -> (z = x) -> x + y + z
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      ir.arguments.length shouldEqual 3
      ir.arguments(2).defaultValue shouldBe defined
    }

    "work properly with usages of shadowed parameters in default arguments" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x -> (y = x) -> (x = x + 1) -> x + y
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      // Usages of `x` and `y` should be untouched
      ir.body
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
        .name shouldEqual "x"
      ir.body
        .asInstanceOf[Application.Prefix]
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
        .name shouldEqual "y"

      // The first argument `x` should be renamed
      val newXName = ir.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name

      newXName should not equal "x"

      // Usages of the first argument `x` should be replaced by the new name
      ir.arguments(1)
        .asInstanceOf[DefinitionArgument.Specified]
        .defaultValue
        .get
        .asInstanceOf[Name.Literal]
        .name shouldEqual newXName
      ir.arguments(2)
        .asInstanceOf[DefinitionArgument.Specified]
        .defaultValue
        .get
        .asInstanceOf[Application.Prefix]
        .arguments
        .head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
        .name shouldEqual newXName
    }

    "maintain laziness of collapsed parameters" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |~x -> ~y -> x + y
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      ir.arguments.length shouldEqual 2
      ir.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .suspended shouldEqual true
      ir.arguments(1)
        .asInstanceOf[DefinitionArgument.Specified]
        .suspended shouldEqual true
    }

    "work properly with arguments defaulted to lambdas" in {
      implicit val inlineContext: InlineContext = mkContext
      val ir = """
                 |x -> (y = x->y->z) -> y x
                 |""".stripMargin.preprocessExpression.get.optimise
        .asInstanceOf[Function.Lambda]
      ir.arguments.length shouldEqual 2
      val defaultExpr = ir.arguments(1).defaultValue.get
      defaultExpr shouldBe a[Function.Lambda]
      defaultExpr
        .asInstanceOf[Function.Lambda]
        .arguments
        .length shouldEqual 2
    }

    "collapse lambdas with multiple parameters" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir: Function.Lambda = Function
        .Lambda(
          List(
            DefinitionArgument
              .Specified(
                Name
                  .Literal("a", isMethod = false, None),
                None,
                None,
                suspended = false,
                None
              ),
            DefinitionArgument.Specified(
              Name.Literal("b", isMethod = false, None),
              None,
              None,
              suspended = false,
              None
            )
          ),
          Function.Lambda(
            List(
              DefinitionArgument.Specified(
                Name
                  .Literal("c", isMethod = false, None),
                None,
                None,
                suspended = false,
                None
              )
            ),
            Name.Literal("c", isMethod = false, None),
            None
          ),
          None
        )
        .runPasses(passManager, inlineContext)
        .optimise
        .asInstanceOf[Function.Lambda]

      ir.arguments.length shouldEqual 3
      ir.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name shouldEqual "a"
      ir.arguments(1)
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name shouldEqual "b"
      ir.arguments(2)
        .asInstanceOf[DefinitionArgument.Specified]
        .name
        .name shouldEqual "c"
    }

    "output a warning when lambda chaining shadows a parameter definition" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x -> x -> x
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      val ws = ir.arguments.head.diagnostics.toList.collect {
        case w: warnings.Shadowed.FunctionParam => w
      }

      ws should not be empty
      ws.head.shadowedName shouldEqual "x"
      ws.head.shadower shouldBe ir.arguments(1)
      ws.head.message(
        null
      ) shouldBe "The argument 'x' is shadowed by another one with the same name."
    }

    "consolidate chained lambdas if the chaining occurs via a single-lined block" in {
      implicit val inlineContext: InlineContext = mkContext

      val ir =
        """
          |x ->
          |    y -> x + y
          |""".stripMargin.preprocessExpression.get.optimise
          .asInstanceOf[Function.Lambda]

      ir.arguments.length shouldEqual 2
    }
  }
}

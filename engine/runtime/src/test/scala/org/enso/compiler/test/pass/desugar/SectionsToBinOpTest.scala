package org.enso.compiler.test.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.pass.desugar.{GenerateMethodBodies, SectionsToBinOp}
import org.enso.compiler.test.CompilerTest

class SectionsToBinOpTest extends CompilerTest {

  // === Test Configuration ===================================================

  val passes: List[IRPass] = List(
    GenerateMethodBodies
  )

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(passes, passConfiguration)

  /** Adds an extension method for running desugaring on the input IR.
    *
    * @param ir the IR to desugar
    */
  implicit class DesugarExpression(ir: IR.Expression) {

    /** Runs section desugaring on [[ir]].
      *
      * @param inlineContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all sections desugared
      */
    def desugar(implicit inlineContext: InlineContext): IR.Expression = {
      SectionsToBinOp.runExpression(ir, inlineContext)
    }
  }

  /** Makes an inline context.
    *
    * @return a new inline context
    */
  def mkInlineContext: InlineContext = {
    InlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Operator section desugaring" should {
    "work for left sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(1 +)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Application.Prefix]
      ir.asInstanceOf[IR.Application.Prefix].arguments.length shouldEqual 1
    }

    "work for sides sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]

      val irLam = ir.asInstanceOf[IR.Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[IR.Application.Prefix]
      lamBody.arguments.length shouldEqual 1
      val lamBodyFirstArg =
        lamBody.arguments.head
          .asInstanceOf[IR.CallArgument.Specified]
          .value
          .asInstanceOf[IR.Name.Literal]

      lamBodyFirstArg.name shouldEqual lamArgName.name
      lamBodyFirstArg.getId should not equal lamArgName.getId
    }

    "work for right sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+ 1)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]

      val irLam = ir.asInstanceOf[IR.Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[IR.Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg =
        lamBody.arguments.head
          .asInstanceOf[IR.CallArgument.Specified]
          .value
          .asInstanceOf[IR.Name.Literal]

      lamBodyFirstArg.name shouldEqual lamArgName.name
      lamBodyFirstArg.getId should not equal lamArgName.getId
    }

    "work when the section is nested" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x -> (x +)
          |""".stripMargin.preprocessExpression.get.desugar
          .asInstanceOf[IR.Function.Lambda]

      ir.body shouldBe an[IR.Application.Prefix]
      ir.body.asInstanceOf[IR.Application.Prefix].arguments.length shouldEqual 1
    }

    "flip the arguments when a right section's argument is a blank" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(- _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]
      val irFn = ir.asInstanceOf[IR.Function.Lambda]

      val rightArgName =
        irFn.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified].name

      irFn.body shouldBe an[IR.Function.Lambda]
      val irFn2 = irFn.body.asInstanceOf[IR.Function.Lambda]

      val leftArgName =
        irFn2.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified].name

      irFn2.body shouldBe an[IR.Application.Prefix]
      val app = irFn2.body.asInstanceOf[IR.Application.Prefix]

      val appLeftName = app.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
        .value
        .asInstanceOf[IR.Name.Literal]
      val appRightName = app
        .arguments(1)
        .value
        .asInstanceOf[IR.Name.Literal]

      leftArgName.name shouldEqual appLeftName.name
      rightArgName.name shouldEqual appRightName.name
    }
  }
}

package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.SectionsToBinOp
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class SectionsToBinOpTest extends CompilerTest {

  // === Test Configuration ===================================================

  val passes = new Passes

  val precursorPasses: PassGroup = passes.getPrecursors(SectionsToBinOp).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

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
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Operator section desugaring" should {
    "work for left sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(1 +)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]
      ir.location shouldBe defined

      val irLam = ir.asInstanceOf[IR.Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[IR.Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg =
        lamBody
          .arguments(1)
          .asInstanceOf[IR.CallArgument.Specified]
          .value
          .asInstanceOf[IR.Name.Literal]

      lamBodyFirstArg.name shouldEqual lamArgName.name
      lamBodyFirstArg.getId should not equal lamArgName.getId
    }

    "work for sides sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]
      // TODO[DB] Section.Sides location is not parsed
      //ir.location shouldBe defined

      val leftLam = ir.asInstanceOf[IR.Function.Lambda]
      leftLam.arguments.length shouldEqual 1
      val leftLamArgName =
        leftLam.arguments.head
          .asInstanceOf[IR.DefinitionArgument.Specified]
          .name

      val rightLam = leftLam.body.asInstanceOf[IR.Function.Lambda]
      rightLam.arguments.length shouldEqual 1
      val rightLamArgName = rightLam.arguments.head
        .asInstanceOf[IR.DefinitionArgument.Specified]
        .name

      val lamBody = rightLam.body.asInstanceOf[IR.Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg = lamBody.arguments.head
        .asInstanceOf[IR.CallArgument.Specified]
        .value
        .asInstanceOf[IR.Name.Literal]
      val lamBodySecondArg = lamBody
        .arguments(1)
        .asInstanceOf[IR.CallArgument.Specified]
        .value
        .asInstanceOf[IR.Name.Literal]

      lamBodyFirstArg.name shouldEqual leftLamArgName.name
      lamBodySecondArg.name shouldEqual rightLamArgName.name
      lamBodyFirstArg.getId should not equal leftLamArgName.getId
      lamBodySecondArg.getId should not equal rightLamArgName.getId
    }

    "work for right sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+ 1)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]
      ir.location shouldBe defined

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

      ir.body
        .asInstanceOf[IR.Function.Lambda]
        .body shouldBe an[IR.Application.Prefix]
      ir.body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Application.Prefix]
        .arguments
        .length shouldEqual 2
    }

    "flip the arguments when a right section's argument is a blank" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+ _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[IR.Function.Lambda]
      ir.location shouldBe defined
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

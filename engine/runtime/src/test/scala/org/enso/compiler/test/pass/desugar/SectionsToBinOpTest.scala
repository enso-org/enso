package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Name
}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.pass.desugar.SectionsToBinOp
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class SectionsToBinOpTest extends CompilerTest {

  // === Test Configuration ===================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(SectionsToBinOp).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for running desugaring on the input IR.
    *
    * @param ir the IR to desugar
    */
  implicit class DesugarExpression(ir: Expression) {

    /** Runs section desugaring on [[ir]].
      *
      * @param inlineContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all sections desugared
      */
    def desugar(implicit inlineContext: InlineContext): Expression = {
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

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined

      val irLam = ir.asInstanceOf[Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg =
        lamBody
          .arguments(1)
          .asInstanceOf[CallArgument.Specified]
          .value
          .asInstanceOf[Name.Literal]

      lamBodyFirstArg.name shouldEqual lamArgName.name
      lamBodyFirstArg.getId should not equal lamArgName.getId
    }

    "work for sides sections" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      // TODO[DB] Section.Sides location is not parsed
      //ir.location shouldBe defined

      val leftLam = ir.asInstanceOf[Function.Lambda]
      leftLam.arguments.length shouldEqual 1
      val leftLamArgName =
        leftLam.arguments.head
          .asInstanceOf[DefinitionArgument.Specified]
          .name

      val rightLam = leftLam.body.asInstanceOf[Function.Lambda]
      rightLam.arguments.length shouldEqual 1
      val rightLamArgName = rightLam.arguments.head
        .asInstanceOf[DefinitionArgument.Specified]
        .name

      val lamBody = rightLam.body.asInstanceOf[Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg = lamBody.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val lamBodySecondArg = lamBody
        .arguments(1)
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]

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

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined

      val irLam = ir.asInstanceOf[Function.Lambda]
      irLam.arguments.length shouldEqual 1

      val lamArgName =
        irLam.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      val lamBody = irLam.body.asInstanceOf[Application.Prefix]
      lamBody.arguments.length shouldEqual 2
      val lamBodyFirstArg =
        lamBody.arguments.head
          .asInstanceOf[CallArgument.Specified]
          .value
          .asInstanceOf[Name.Literal]

      lamBodyFirstArg.name shouldEqual lamArgName.name
      lamBodyFirstArg.getId should not equal lamArgName.getId
    }

    "work when the section is nested" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |x -> (x +)
          |""".stripMargin.preprocessExpression.get.desugar
          .asInstanceOf[Function.Lambda]

      ir.body
        .asInstanceOf[Function.Lambda]
        .body shouldBe an[Application.Prefix]
      ir.body
        .asInstanceOf[Function.Lambda]
        .body
        .asInstanceOf[Application.Prefix]
        .arguments
        .length shouldEqual 2
    }

    "flip the arguments when a right section's argument is a blank" in {
      implicit val ctx: InlineContext = mkInlineContext

      val ir =
        """
          |(+ _)
          |""".stripMargin.preprocessExpression.get.desugar

      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined
      val irFn = ir.asInstanceOf[Function.Lambda]

      val rightArgName =
        irFn.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn.body shouldBe an[Function.Lambda]
      val irFn2 = irFn.body.asInstanceOf[Function.Lambda]

      val leftArgName =
        irFn2.arguments.head.asInstanceOf[DefinitionArgument.Specified].name

      irFn2.body shouldBe an[Application.Prefix]
      val app = irFn2.body.asInstanceOf[Application.Prefix]

      val appLeftName = app.arguments.head
        .asInstanceOf[CallArgument.Specified]
        .value
        .asInstanceOf[Name.Literal]
      val appRightName = app
        .arguments(1)
        .value
        .asInstanceOf[Name.Literal]

      leftArgName.name shouldEqual appLeftName.name
      rightArgName.name shouldEqual appRightName.name
    }
  }
}

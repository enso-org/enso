package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Function,
  Literal,
  Name
}
import org.enso.compiler.pass.{
  IRPass,
  MiniPassFactory,
  PassConfiguration,
  PassGroup,
  PassManager
}

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.pass.desugar.SectionsToBinOp

import org.enso.compiler.test.MiniPassTest

class SectionsToBinOpTest extends MiniPassTest {
  override def testName: String                 = "Section To Bin Op"
  override def miniPassFactory: MiniPassFactory = SectionsToBinOp.INSTANCE

  override def megaPass: IRPass = SectionsToBinOpMegaPass

  override def megaPassManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  // === Test Setup ===========================================================

  val passes                               = new Passes(defaultConfig)
  val passConfiguration: PassConfiguration = PassConfiguration()
  val precursorPasses: PassGroup =
    passes.getPrecursors(SectionsToBinOp.INSTANCE).get

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  def mkInlineContext: InlineContext = {
    buildInlineContext(freshNameSupply = Some(new FreshNameSupply))
  }

  // === The Tests ============================================================

  "Operator section desugaring" should {

    "work for left sections" in {
      val code = """
                   |(1 +)
                   |""".stripMargin

      assertInlineCompilation(
        code,
        () => mkInlineContext,
        assertWorkForLeftSection,
        true
      )
    }

    def assertWorkForLeftSection(ir: IR) = {
      ir shouldBe an[Application.Prefix]
      ir.location shouldBe defined

      val irApp = ir.asInstanceOf[Application.Prefix]
      irApp.arguments.length shouldEqual 1
      irApp.arguments.head shouldBe an[CallArgument.Specified]

      val irAppArgument =
        irApp.arguments.head.asInstanceOf[CallArgument.Specified]
      irAppArgument.value shouldBe an[Literal.Number]

      irApp.function shouldBe an[Name.Literal]
      val irAppFunction = irApp.function.asInstanceOf[Name.Literal]
      irAppFunction.name shouldEqual "+"
    }

    "work for sides sections" in {
      val code =
        """
          |(+)
          |""".stripMargin

      assertInlineCompilation(
        code,
        () => mkInlineContext,
        assertWorksForSidesSection,
        true
      )
    }

    def assertWorksForSidesSection(ir: IR) = {
      ir shouldBe an[Function.Lambda]
      ir.location shouldBe defined

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
      lamBodyFirstArg.getId() should not equal leftLamArgName.getId()
      lamBodySecondArg.getId() should not equal rightLamArgName.getId()
    }

    "work for right sections" in {
      val code =
        """
          |(+ 1)
          |""".stripMargin

      assertInlineCompilation(
        code,
        () => mkInlineContext,
        assertWorkForRightSections,
        true
      )

    }

    def assertWorkForRightSections(ir: IR) = {
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
      lamBodyFirstArg.getId() should not equal lamArgName.getId()
    }

    "work when the section is nested" in {
      val code =
        """
          |x -> (x +)
          |""".stripMargin

      assertInlineCompilation(
        code,
        () => mkInlineContext,
        assertWorkWhenTheSectionIsNested,
        true
      )
    }

    def assertWorkWhenTheSectionIsNested(x: IR) = {
      val ir = x.asInstanceOf[Function.Lambda]

      ir.body
        .asInstanceOf[Application.Prefix]
        .function shouldBe an[Name.Literal]
      ir.body
        .asInstanceOf[Application.Prefix]
        .arguments
        .length shouldEqual 1
    }

    "flip the arguments when a right section's argument is a blank" in {
      val code =
        """
          |(+ _)
          |""".stripMargin

      assertInlineCompilation(
        code,
        () => mkInlineContext,
        assertFlipTheArgumentsWhenARightSection,
        true
      )

    }

    def assertFlipTheArgumentsWhenARightSection(ir: IR) = {
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

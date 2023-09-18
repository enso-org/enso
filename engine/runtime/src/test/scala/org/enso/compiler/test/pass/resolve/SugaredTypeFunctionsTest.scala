package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.ir.{Expression, Function, Type}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.`type`
import org.enso.compiler.pass.resolve.TypeFunctions
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class SugaredTypeFunctionsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(TypeFunctions).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to resolve typing functions to an expression.
    *
    * @param ir the expression to resolve typing functions in
    */
  implicit class ResolveExpression(ir: Expression) {

    /** Resolves typing functions on [[ir]].
      *
      * @param inlineContext the context win which resolution takes place
      * @return [[ir]], with typing functions resolved
      */
    def resolve(implicit inlineContext: InlineContext): Expression = {
      TypeFunctions.runExpression(ir, inlineContext)
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

  "Type functions resolution" should {
    implicit val ctx: InlineContext = mkInlineContext

    "work for saturated applications" in {
      val ir =
        """
          |a : B
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[Type.Ascription]
    }

    "work for left sections" in {
      val ir =
        """
          |(a :)
          |""".stripMargin.preprocessExpression.get.resolve

      if (!ir.isInstanceOf[errors.Syntax]) {
        ir shouldBe an[Function.Lambda]
        ir.asInstanceOf[Function.Lambda].body shouldBe an[Type.Ascription]
      }
    }

    "work for centre sections" in {
      val ir =
        """
          |(:)
          |""".stripMargin.preprocessExpression.get.resolve

      if (!ir.isInstanceOf[errors.Syntax]) {
        ir shouldBe an[Function.Lambda]
        ir.asInstanceOf[Function.Lambda]
          .body
          .asInstanceOf[Function.Lambda]
          .body shouldBe an[Type.Ascription]
      }
    }

    "work for right sections" in {
      val ir =
        """
          |(: a)
          |""".stripMargin.preprocessExpression.get.resolve

      if (!ir.isInstanceOf[errors.Syntax]) {
        ir shouldBe an[Function.Lambda]
        ir.asInstanceOf[Function.Lambda].body shouldBe an[Type.Ascription]
      }
    }

    "work for underscore arguments on the left" in {
      val ir =
        """
          |_ : A
          |""".stripMargin.preprocessExpression.get.resolve

      if (!ir.isInstanceOf[errors.Syntax]) {
        ir shouldBe an[Type.Ascription]
      }
    }

    "work for underscore arguments on the right" in {
      val ir =
        """
          |a : _
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[Type.Ascription]
    }
  }

  "Resolution" should {
    implicit val ctx: InlineContext = mkInlineContext

    "resolve type ascription" in {
      val ir =
        """
          |a : A
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[Type.Ascription]
    }

    "resolve context ascription" ignore {
      // FIXME: Not supported by new parser--needs triage (#6165).
      val ir =
        """
          |a in IO
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[Type.Context]
    }

    "resolve error ascription" in {
      val ir =
        """
          |IO ! Error
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[Type.Error]
    }

    "resolve subsumption" in {
      val ir =
        """
          |T <: P
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[`type`.Set.Subsumption]
    }

    "resolve equality" ignore {
      // FIXME: Not supported by new parser--needs triage (#6165).
      val ir =
        """
          |T ~ P
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[`type`.Set.Equality]
    }

    "resolve concatenation" in {
      val ir =
        """
          |T ; P
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[`type`.Set.Concat]
    }

    "resolve union" in {
      val ir =
        """
          |T | P
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[`type`.Set.Union]
    }

    "resolve intersection" in {
      val ir =
        """
          |T & P
          |""".stripMargin.preprocessExpression.get.resolve

      ir shouldBe an[`type`.Set.Intersection]
    }
  }
}

package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.resolve.IgnoredBindings
import org.enso.compiler.pass.resolve.IgnoredBindings.State
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest

class IgnoredBindingsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: List[IRPass] = passes.getPrecursors(IgnoredBindings).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(precursorPasses, passConfiguration)

  /** Adds an extension method for running desugaring on the input IR.
    *
    * @param ir the IR to desugar
    */
  implicit class DesugarExpression(ir: IR.Expression) {

    /** Runs ignores desugaring on [[ir]].
      *
      * @param inlineContext the inline context in which the desugaring takes
      *                      place
      * @return [[ir]], with all ignores desugared
      */
    def desugar(implicit inlineContext: InlineContext): IR.Expression = {
      IgnoredBindings.runExpression(ir, inlineContext)
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

  "Ignored bindings desugaring for function args" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |_ -> (x = _ -> 1) -> x
        |""".stripMargin.preprocessExpression.get.desugar
        .asInstanceOf[IR.Function.Lambda]
    val blankArg =
      ir.arguments.head.asInstanceOf[IR.DefinitionArgument.Specified]
    val xArg = ir.body
      .asInstanceOf[IR.Function.Lambda]
      .arguments
      .head
      .asInstanceOf[IR.DefinitionArgument.Specified]

    "replace ignored arguments with fresh names" in {
      blankArg.name shouldBe an[IR.Name.Literal]
    }

    "mark ignored arguments as ignored" in {
      blankArg.getMetadata(IgnoredBindings) shouldEqual Some(State.Ignored)
    }

    "mark normal arguments as not ignored" in {
      xArg.getMetadata(IgnoredBindings) shouldEqual Some(State.NotIgnored)
    }

    "work when deeply nested" in {
      val nestedIgnore = xArg.defaultValue.get
        .asInstanceOf[IR.Function.Lambda]
        .arguments
        .head
        .asInstanceOf[IR.DefinitionArgument.Specified]

      nestedIgnore.name shouldBe an[IR.Name.Literal]
      nestedIgnore.getMetadata(IgnoredBindings) shouldEqual Some(State.Ignored)
    }
  }

  "Ignored bindings desugaring for bindings" should {
    implicit val ctx: InlineContext = mkInlineContext

    val ir =
      """
        |_ =
        |    _ = f a b
        |    x = y
        |    10
        |""".stripMargin.preprocessExpression.get.desugar
        .asInstanceOf[IR.Expression.Binding]

    val bindingName = ir.name
    val bindingBody = ir.expression.asInstanceOf[IR.Expression.Block]

    "replace the ignored binding with a fresh name" in {
      bindingName shouldBe an[IR.Name.Literal]
    }

    "mark the binding as ignored if it was" in {
      ir.getMetadata(IgnoredBindings) shouldEqual Some(State.Ignored)
    }

    "mark the binding as not ignored if it wasn't" in {
      val nonIgnored =
        bindingBody.expressions(1).asInstanceOf[IR.Expression.Binding]

      nonIgnored.getMetadata(IgnoredBindings) shouldEqual Some(
        State.NotIgnored
      )
    }

    "work when deeply nested" in {
      val ignoredInBlock =
        bindingBody.expressions.head.asInstanceOf[IR.Expression.Binding]

      ignoredInBlock.name shouldBe an[IR.Name.Literal]
    }
  }
}

package org.enso.compiler.test.pass.optimise

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.compiler.pass.optimise.ApplicationSaturation
import org.enso.compiler.pass.optimise.ApplicationSaturation.{
  CallSaturation,
  FunctionSpec,
  Metadata
}
import org.enso.compiler.pass.{PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.node.ExpressionNode
import org.enso.interpreter.runtime.callable.argument.CallArgument
import org.enso.interpreter.runtime.scope.LocalScope

import scala.annotation.unused

class ApplicationSaturationTest extends CompilerTest {

  // === Utilities ============================================================

  /** Generates n arguments that _do not_ contain function applications
    * themselves.
    *
    * @param n the number of arguments to generate
    * @param positional whether or not the arguments should be generated by name
    *                   or positionally
    * @return a list containing `n` arguments
    */
  def genNArgs(n: Int, positional: Boolean = true): List[IR.CallArgument] = {
    val name = if (positional) {
      None
    } else {
      Some(IR.Name.Literal("a", None))
    }

    List.fill(n)(IR.CallArgument.Specified(name, IR.Empty(None), None))
  }

  // === Test Setup ===========================================================

  // The functions are unused, so left undefined for ease of testing
  def dummyFn(@unused args: List[CallArgument]): ExpressionNode = ???

  val knownFunctions: ApplicationSaturation.Configuration =
    ApplicationSaturation.Configuration(
      Map(
        "+"   -> FunctionSpec(2, dummyFn),
        "baz" -> FunctionSpec(3, dummyFn),
        "foo" -> FunctionSpec(4, dummyFn)
      )
    )

  val passes: Passes = new Passes

  val precursorPasses = passes.getPrecursors(ApplicationSaturation).get

  val knownPassConfig: PassConfiguration = PassConfiguration(
    ApplicationSaturation -->> knownFunctions,
    AliasAnalysis         -->> AliasAnalysis.Configuration()
  )
  val passManagerKnown = new PassManager(List(precursorPasses), knownPassConfig)

  val localScope: Option[LocalScope] = Some(LocalScope.root)

  val knownCtx = new InlineContext(
    localScope        = localScope,
    freshNameSupply   = Some(new FreshNameSupply),
    passConfiguration = Some(knownPassConfig)
  )

  val moduleCtx = new ModuleContext(
    passConfiguration = Some(knownPassConfig),
    freshNameSupply   = Some(new FreshNameSupply)
  )

  // === The Tests ============================================================

  "Known applications" should {
    val plusFn = IR.Application
      .Prefix(
        IR.Name.Literal("+", None),
        genNArgs(2),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    val bazFn = IR.Application
      .Prefix(
        IR.Name.Literal("baz", None),
        genNArgs(2),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    val fooFn = IR.Application
      .Prefix(
        IR.Name.Literal("foo", None),
        genNArgs(5),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    val fooFnByName = IR.Application
      .Prefix(
        IR.Name.Literal("foo", None),
        genNArgs(4, positional = false),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    "be tagged with full saturation where possible" in {
      val resultIR =
        ApplicationSaturation.runExpression(plusFn, knownCtx)

      resultIR.getMetadata(ApplicationSaturation).foreach {
        case _: CallSaturation.Exact => succeed
        case _                       => fail()
      }
    }

    "be tagged with partial saturation where possible" in {
      val resultIR =
        ApplicationSaturation.runExpression(bazFn, knownCtx)
      val expected = Some(CallSaturation.Partial(1))

      resultIR.getMetadata(ApplicationSaturation) shouldEqual expected
    }

    "be tagged with over saturation where possible" in {
      val resultIR =
        ApplicationSaturation.runExpression(fooFn, knownCtx)
      val expected = Some(CallSaturation.Over(1))

      resultIR.getMetadata(ApplicationSaturation) shouldEqual expected
    }

    "be tagged with by name if applied by name" in {
      val resultIR =
        ApplicationSaturation.runExpression(fooFnByName, knownCtx)
      val expected = Some(CallSaturation.ExactButByName())

      resultIR.getMetadata(ApplicationSaturation) shouldEqual expected
    }
  }

  "Unknown applications" should {
    val unknownFn = IR.Application
      .Prefix(
        IR.Name.Literal("unknown", None),
        genNArgs(10),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    "be tagged with unknown saturation" in {
      val resultIR =
        ApplicationSaturation.runExpression(unknownFn, knownCtx)
      val expected = Some(CallSaturation.Unknown())

      resultIR.getMetadata(ApplicationSaturation) shouldEqual expected
    }
  }

  "Known applications containing known applications" should {
    val empty = IR.Empty(None)
    val knownPlus = IR.Application
      .Prefix(
        IR.Name.Literal("+", None),
        genNArgs(2),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    val undersaturatedPlus = IR.Application
      .Prefix(
        IR.Name.Literal("+", None),
        genNArgs(1),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    val oversaturatedPlus = IR.Application
      .Prefix(
        IR.Name.Literal("+", None),
        genNArgs(3),
        hasDefaultsSuspended = false,
        None
      )
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Application.Prefix]

    implicit class InnerMeta(ir: IR.Expression) {
      def getInnerMetadata: Option[Metadata] = {
        ir.asInstanceOf[IR.Application.Prefix]
          .arguments
          .head
          .asInstanceOf[IR.CallArgument.Specified]
          .value
          .getMetadata(ApplicationSaturation)
      }
    }

    def outerPlus(argExpr: IR.Expression): IR.Application.Prefix = {
      IR.Application
        .Prefix(
          IR.Name.Literal("+", None),
          List(
            IR.CallArgument.Specified(None, argExpr, None),
            IR.CallArgument.Specified(None, empty, None)
          ),
          hasDefaultsSuspended = false,
          None
        )
        .runPasses(passManagerKnown, knownCtx)
        .asInstanceOf[IR.Application.Prefix]
    }

    "have fully saturated applications tagged correctly" in {
      val result =
        ApplicationSaturation.runExpression(outerPlus(knownPlus), knownCtx)

      // The outer should be reported as fully saturated
      result.getMetadata(ApplicationSaturation).foreach {
        case _: CallSaturation.Exact => succeed
        case _                       => fail()
      }

      // The inner should be reported as fully saturated
      result.getInnerMetadata.foreach {
        case _: CallSaturation.Exact => succeed
        case _                       => fail()
      }
    }

    "have non-fully saturated applications tagged correctly" in {
      val result =
        ApplicationSaturation.runExpression(
          outerPlus(undersaturatedPlus),
          knownCtx
        )
      val expectedInnerMeta = CallSaturation.Partial(1)

      // The outer should be reported as fully saturated
      result.getMetadata(ApplicationSaturation).foreach {
        case _: CallSaturation.Exact => succeed
        case _                       => fail()
      }

      // The inner should be reported as under saturateD
      result.getInnerMetadata
        .foreach(t => t shouldEqual expectedInnerMeta)
    }

    "have a mixture of application saturations tagged correctly" in {
      val result =
        ApplicationSaturation.runExpression(
          outerPlus(oversaturatedPlus),
          knownCtx
        )
      val expectedInnerMeta = CallSaturation.Over(1)

      // The outer should be reported as fully saturated
      result.getMetadata(ApplicationSaturation).foreach {
        case _: CallSaturation.Exact => succeed
        case _                       => fail()
      }

      // The inner should be reported as under saturateD
      result.getInnerMetadata
        .foreach(t => t shouldEqual expectedInnerMeta)
    }
  }

  "Shadowed known functions" should {
    val rawIR =
      """
        |main =
        |    foo = x -> y -> z -> x + y + z
        |
        |    foo a b c
        |""".stripMargin.toIrExpression

    val inputIR = rawIR.get
      .runPasses(passManagerKnown, knownCtx)
      .asInstanceOf[IR.Expression]

    val result = ApplicationSaturation
      .runExpression(inputIR, knownCtx)
      .asInstanceOf[IR.Expression.Binding]

    "be tagged as unknown even if their name is known" in {
      // Needs alias analysis to work
      result.expression
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .getMetadata(ApplicationSaturation)
        .foreach {
          case _: CallSaturation.Unknown => succeed
          case _                         => fail()
        }
    }
  }

}

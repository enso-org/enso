package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.{
  CallArgument,
  Empty,
  Expression,
  IdentifiedLocation,
  Location,
  Name
}
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis
}
import org.enso.compiler.pass.{
  IRPass,
  IRProcessingPass,
  MiniIRPass,
  MiniPassFactory,
  PassConfiguration,
  PassManager
}
import org.enso.compiler.pass.desugar.{
  GenerateMethodBodies,
  OperatorToFunction,
  SectionsToBinOp
}
import org.enso.compiler.test.MiniPassTest

class OperatorToFunctionTest extends MiniPassTest {
  override def testName: String = "OperatorToFunction"

  override def miniPassFactory: MiniPassFactory = OperatorToFunction

  override def megaPass: IRPass = OperatorToFunctionTestPass

  override def megaPassManager: PassManager = {
    val passes     = new Passes(defaultConfig)
    val precursors = passes.getPrecursors(OperatorToFunction).get
    new PassManager(List(precursors), PassConfiguration())
  }

  // === Utilities ============================================================

  val ctx    = buildInlineContext()
  val modCtx = buildModuleContext()

  /** Generates an operator and its corresponding function.
    *
    * @param name the name of the operator
    * @param left the left expression
    * @param right the right expression
    * @return an operator `name` and its corresponding function
    */
  def genOprAndFn(
    name: Name,
    left: Expression,
    right: Expression
  ): (Operator.Binary, Application.Prefix) = {
    val loc = new IdentifiedLocation(new Location(1, 33))

    val leftArg = CallArgument.Specified(None, left, left.identifiedLocation())
    val rightArg =
      CallArgument.Specified(None, right, right.identifiedLocation())

    val binOp =
      Operator.Binary(leftArg, name, rightArg, loc)
    val opFn = Application.Prefix(
      name,
      List(leftArg, rightArg),
      hasDefaultsSuspended = false,
      loc
    )

    (binOp, opFn)
  }

  // === The Tests ============================================================
  val opName =
    Name.Literal("=:=", isMethod = true, null)
  val left     = Empty(null)
  val right    = Empty(null)
  val rightArg = CallArgument.Specified(None, Empty(null), null)

  val (operator, operatorFn) = genOprAndFn(opName, left, right)

  val oprArg   = CallArgument.Specified(None, operator, null)
  val oprFnArg = CallArgument.Specified(None, operatorFn, null)

  "Operators" should {
    val opName =
      Name.Literal("=:=", isMethod = true, identifiedLocation = null)
    val left  = Empty(identifiedLocation = null)
    val right = Empty(identifiedLocation = null)
    val rightArg = CallArgument.Specified(
      None,
      Empty(identifiedLocation = null),
      identifiedLocation = null
    )

    val (operator, operatorFn) = genOprAndFn(opName, left, right)

    val oprArg =
      CallArgument.Specified(None, operator, identifiedLocation = null)
    val oprFnArg =
      CallArgument.Specified(None, operatorFn, identifiedLocation = null)

    "be translated to functions" in {
      OperatorToFunctionTestPass.runExpression(
        operator,
        ctx
      ) shouldEqual operatorFn
    }

    "be translated recursively in synthetic IR" in {
      val recursiveIR =
        Operator.Binary(oprArg, opName, rightArg, null)
      val recursiveIRResult = Application.Prefix(
        opName,
        List(oprFnArg, rightArg),
        hasDefaultsSuspended = false,
        null
      )

      OperatorToFunctionTestPass.runExpression(
        recursiveIR,
        ctx
      ) shouldEqual recursiveIRResult
    }

    "be translated recursively" in {
      val code =
        """
          |main =
          |    a = 1 + 2
          |    nested_method x y = x + y
          |    nested_method (3 * 4) a
          |""".stripMargin
      assertModuleCompilation(
        code,
        () =>
          buildModuleContext(
            freshNameSupply = Some(new FreshNameSupply())
          ),
        ir => {
          ir.preorder().foreach {
            case _: Operator.Binary => fail("Operator.Binary found")
            case _                  =>
          }
        }
      )
    }
  }

  "Operators mini pass" should {
    "be translated to functions" in {
      val miniPass = OperatorToFunction.createForInlineCompilation(ctx)
      val miniRes =
        MiniIRPass.compile(classOf[Expression], operator, miniPass)
      miniRes shouldEqual operatorFn
    }

    "be translated recursively" in {
      val recursiveIR =
        Operator.Binary(oprArg, opName, rightArg, identifiedLocation = null)
      val recursiveIRResult = Application.Prefix(
        opName,
        List(oprFnArg, rightArg),
        hasDefaultsSuspended = false,
        identifiedLocation   = null
      )

      val miniPass = OperatorToFunction.createForInlineCompilation(ctx)
      val miniRes =
        MiniIRPass.compile(classOf[Expression], recursiveIR, miniPass)
      miniRes shouldEqual recursiveIRResult
    }
  }
}

/** Copied from the original implementation in `OperatorToFunction`
  * This pass converts usages of operators to calls to standard functions.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object OperatorToFunctionTestPass extends IRPass {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    GenerateMethodBodies,
    SectionsToBinOp.INSTANCE
  )
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    DemandAnalysis
  )

  /** Executes the conversion pass.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val new_bindings = ir.bindings.map { a =>
      a.mapExpressions(
        runExpression(
          _,
          new InlineContext(
            moduleContext,
            compilerConfig = moduleContext.compilerConfig
          )
        )
      )
    }
    ir.copy(bindings = new_bindings)
  }

  /** Executes the conversion pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    ir.transformExpressions { case operatorBinary: Operator.Binary =>
      new Application.Prefix(
        operatorBinary.operator,
        List(
          operatorBinary.left.mapExpressions(runExpression(_, inlineContext)),
          operatorBinary.right.mapExpressions(runExpression(_, inlineContext))
        ),
        hasDefaultsSuspended = false,
        operatorBinary.location.orNull,
        operatorBinary.passData,
        operatorBinary.diagnostics
      )
    }
  }
}

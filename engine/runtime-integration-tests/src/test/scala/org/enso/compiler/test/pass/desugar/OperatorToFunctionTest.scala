package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  Empty,
  Expression,
  IdentifiedLocation,
  Location,
  Module,
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

    val leftArg = CallArgument.Specified
      .builder()
      .name(None)
      .value(left)
      .isSynthetic(false)
      .location(left.identifiedLocation())
      .build()

    val rightArg =
      CallArgument.Specified
        .builder()
        .name(None)
        .value(right)
        .isSynthetic(false)
        .location(right.identifiedLocation())
        .build()

    val binOp = Operator.Binary
      .builder()
      .left(leftArg)
      .operator(name)
      .right(rightArg)
      .location(loc)
      .build()
    val opFn = Application.Prefix
      .builder()
      .function(name)
      .arguments(List(leftArg, rightArg))
      .hasDefaultsSuspended(false)
      .location(loc)
      .build()

    (binOp, opFn)
  }

  // === The Tests ============================================================
  val opName =
    Name.Literal("=:=", isMethod = true, null)
  val left  = new Empty(null)
  val right = new Empty(null)
  val rightArg = CallArgument.Specified
    .builder()
    .name(None)
    .value(new Empty(null))
    .isSynthetic(false)
    .build()

  val (operator, operatorFn) = genOprAndFn(opName, left, right)

  val oprArg =
    CallArgument.Specified
      .builder()
      .name(None)
      .value(operator)
      .isSynthetic(false)
      .build()
  val oprFnArg =
    CallArgument.Specified
      .builder()
      .name(None)
      .value(operatorFn)
      .isSynthetic(false)
      .build()

  "Operators" should {
    val opName =
      Name.Literal("=:=", isMethod = true, identifiedLocation = null)
    val left  = new Empty(null)
    val right = new Empty(null)
    val rightArg =
      CallArgument.Specified
        .builder()
        .name(None)
        .value(new Empty(null))
        .isSynthetic(false)
        .build()

    val (operator, operatorFn) = genOprAndFn(opName, left, right)

    val oprArg =
      CallArgument.Specified
        .builder()
        .name(None)
        .value(operator)
        .isSynthetic(false)
        .build()
    val oprFnArg =
      CallArgument.Specified
        .builder()
        .name(None)
        .value(operatorFn)
        .isSynthetic(false)
        .build()

    "be translated to functions" in {
      OperatorToFunctionTestPass.runExpression(
        operator,
        ctx
      ) shouldEqual operatorFn
    }

    "be translated recursively in synthetic IR" in {
      val recursiveIR = Operator.Binary
        .builder()
        .left(oprArg)
        .operator(opName)
        .right(rightArg)
        .build()
      val recursiveIRResult = Application.Prefix
        .builder()
        .function(opName)
        .arguments(List(oprFnArg, rightArg))
        .build()

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
      val recursiveIR = Operator.Binary
        .builder()
        .left(oprArg)
        .operator(opName)
        .right(rightArg)
        .build()
      val recursiveIRResult = Application.Prefix
        .builder()
        .function(opName)
        .arguments(List(oprFnArg, rightArg))
        .build()

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
    ir.copyWithBindings(new_bindings)
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
      Application.Prefix
        .builder()
        .function(operatorBinary.operator)
        .arguments(
          List(
            operatorBinary.left.mapExpressions(runExpression(_, inlineContext)),
            operatorBinary.right.mapExpressions(runExpression(_, inlineContext))
          )
        )
        .hasDefaultsSuspended(false)
        .location(operatorBinary.location().orNull)
        .passData(operatorBinary.passData)
        .diagnostics(operatorBinary.diagnostics)
        .build()
    }
  }
}

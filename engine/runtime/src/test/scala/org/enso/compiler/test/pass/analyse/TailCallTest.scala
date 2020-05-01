package org.enso.compiler.test.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.analyse.TailCall.TailPosition
import org.enso.compiler.pass.analyse.{AliasAnalysis, TailCall}
import org.enso.compiler.pass.desugar.{
  GenerateMethodBodies,
  LiftSpecialOperators,
  OperatorToFunction
}
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.interpreter.runtime.scope.LocalScope
import org.enso.compiler.pass.PassConfiguration._

class TailCallTest extends CompilerTest {

  // === Test Setup ===========================================================

  val modCtx: ModuleContext = ModuleContext()

  val tailCtx: InlineContext = InlineContext(
    localScope       = Some(LocalScope.root),
    isInTailPosition = Some(true)
  )

  val noTailCtx: InlineContext = InlineContext(
    localScope       = Some(LocalScope.root),
    isInTailPosition = Some(false)
  )

  val precursorPasses: List[IRPass] = List(
    GenerateMethodBodies,
    LiftSpecialOperators,
    OperatorToFunction,
    AliasAnalysis
  )

  val passConfiguration: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
  )

  val passManager = new PassManager(precursorPasses, passConfiguration)

  /** Adds an extension method to preprocess source code as an Enso module.
    *
    * @param code the source code to preprocess
    */
  implicit class PreprocessModule(code: String) {

    /** Preprocesses the provided source code into an [[IR.Module]].
      *
      * @return the IR representation of [[code]]
      */
    def runTCAModule: IR.Module = {
      val preprocessed = code.toIrModule
        .runPasses(passManager, ModuleContext())
        .asInstanceOf[IR.Module]

      TailCall.runModule(preprocessed, modCtx)
    }
  }

  /** Adds an extension method to preprocess source code as an Enso expression.
    *
    * @param code the source code to preprocess
    */
  implicit class PreprocessExpression(code: String) {

    /** Preprocesses the provided source code into an [[IR.Expression]].
      *
      * @return the IR representation of [[code]]
      */
    def runTCAExpression(context: InlineContext): IR.Expression = {
      val preprocessed = code.toIrExpression
        .getOrElse(
          throw new CompilerError("Code was not a valid expression.")
        )
        .runPasses(passManager, context)
        .asInstanceOf[IR.Expression]

      TailCall.runExpression(preprocessed, context)
    }
  }

  // === The Tests ============================================================

  "Tail call analysis on modules" should {
    val ir =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |
        |    case c of
        |      Baz a b -> a * b * d
        |      _ -> d
        |
        |type MyAtom a b c
        |""".stripMargin.runTCAModule

    "mark methods as tail" in {
      ir.bindings.head
        .getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "mark atoms as tail" in {
      ir.bindings(1)
        .getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }
  }

  "Tail call analysis on expressions" should {
    val code =
      """
        |x -> y -> z -> x y z
        |""".stripMargin

    "mark the expression as tail if the context requires it" in {
      val ir = code.runTCAExpression(tailCtx)

      ir.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "not mark the expression as tail if the context doesn't require it" in {
      val ir = code.runTCAExpression(noTailCtx)

      ir.getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
    }
  }

  "Tail call analysis on functions" should {
    val ir =
      """
        |a -> b -> c ->
        |    d = a + b
        |    e = a * c
        |    d + e
        |""".stripMargin
        .runTCAExpression(tailCtx)
        .asInstanceOf[IR.Function.Lambda]

    val fnBody = ir.body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Expression.Block]

    "mark the last expression of the function as tail" in {
      fnBody.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the other expressions in the function as not tail" in {
      fnBody.expressions.foreach(expr =>
        expr.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      )
    }
  }

  "Tail call analysis on case expressions" should {
    "not mark any portion of the branch functions as tail by default" in {
      val ir =
        """
          |Foo.bar = a ->
          |    x = case a of
          |        Lambda fn arg -> fn arg
          |
          |    x
          |""".stripMargin.runTCAModule

      val caseExpr = ir.bindings.head
        .asInstanceOf[Method]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .expressions
        .head
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Case.Expr]

      caseExpr.getMetadata(TailCall) shouldEqual Some(
        TailPosition.NotTail
      )
      caseExpr.branches.foreach(branch => {
        val branchExpression =
          branch.expression.asInstanceOf[IR.Function.Lambda]

        branchExpression.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
        branchExpression.body.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      })
    }

    "only mark the branches as tail if the expression is in tail position" in {
      val ir =
        """
          |Foo.bar = a ->
          |    case a of
          |      Lambda fn arg -> fn arg
          |""".stripMargin.runTCAModule

      val caseExpr = ir.bindings.head
        .asInstanceOf[Method]
        .body
        .asInstanceOf[IR.Function.Lambda]
        .body
        .asInstanceOf[IR.Expression.Block]
        .returnValue
        .asInstanceOf[IR.Case.Expr]

      caseExpr.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
      caseExpr.branches.foreach(branch => {
        val branchExpression =
          branch.expression.asInstanceOf[IR.Function.Lambda]

        branchExpression.getMetadata(TailCall) shouldEqual Some(
          TailPosition.Tail
        )
        branchExpression.body.getMetadata(TailCall) shouldEqual Some(
          TailPosition.Tail
        )
      })
    }
  }

  "Tail call analysis on function calls" should {
    val tailCall =
      """
        |Foo.bar =
        |   IO.println "AAAAA"
        |""".stripMargin.runTCAModule.bindings.head.asInstanceOf[Method]
    val tailCallBody = tailCall.body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Expression.Block]

    val nonTailCall =
      """
        |Foo.bar =
        |    a = b c d
        |    a
        |""".stripMargin.runTCAModule.bindings.head.asInstanceOf[Method]
    val nonTailCallBody = nonTailCall.body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Expression.Block]

    "mark the arguments as tail" in {
      nonTailCallBody.expressions.head
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Application.Prefix]
        .arguments
        .foreach(arg =>
          arg.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        )

      tailCallBody.returnValue
        .asInstanceOf[IR.Application.Prefix]
        .arguments
        .foreach(arg =>
          arg.getMetadata(TailCall) shouldEqual Some(
            TailPosition.Tail
          )
        )
    }

    "mark the function call as tail if it is in a tail position" in {
      tailCallBody.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the function call as not tail if it is in a tail position" in {
      nonTailCallBody.expressions.head
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .getMetadata(TailCall) shouldEqual Some(TailPosition.NotTail)
    }
  }

  "Tail call analysis on blocks" should {
    val ir =
      """
        |Foo.bar = a -> b -> c ->
        |    d = a + b
        |    mul = a -> b -> a * b
        |    mul c d
        |""".stripMargin.runTCAModule.bindings.head.asInstanceOf[Method]

    val block = ir.body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Function.Lambda]
      .body
      .asInstanceOf[IR.Expression.Block]

    "mark the bodies of bound functions as tail properly" in {
      block
        .expressions(1)
        .asInstanceOf[IR.Expression.Binding]
        .expression
        .asInstanceOf[IR.Function.Lambda]
        .body
        .getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }

    "mark the block expressions as not tail" in {
      block.expressions.foreach(expr =>
        expr.getMetadata(TailCall) shouldEqual Some(
          TailPosition.NotTail
        )
      )
    }

    "mark the final expression of the block as tail" in {
      block.returnValue.getMetadata(TailCall) shouldEqual Some(
        TailPosition.Tail
      )
    }

    "mark the block as tail if it is in a tail position" in {
      block.getMetadata(TailCall) shouldEqual Some(TailPosition.Tail)
    }
  }
}

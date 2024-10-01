package org.enso.compiler.test.core.ir

import org.enso.compiler.context.InlineContext
import org.enso.compiler.core.ir.CallArgument
import org.enso.compiler.core.ir.Name.Literal
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.pass.{PassConfiguration, PassManager}
import org.enso.compiler.test.CompilerTest
import org.scalatest.matchers.should.Matchers

class IRTest extends CompilerTest with Matchers {
  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(), passConfig)

  private def mkInlineContext: InlineContext = {
    buildInlineContext()
  }

  "Children IR" should {
    "not work for IR with no children" in {
      val lit = Literal("lit", isMethod = false, location = None)
      lit.children() should have size 0
      val otherLit = Literal("other_lit", isMethod = false, location = None)
      a[IllegalArgumentException] should be thrownBy (lit.withNewChildren(
        List(otherLit)
      ))
    }

    "work on IR with single child" in {
      val argLiteral = Literal("arg", isMethod = false, location = None)
      val callArg    = CallArgument.Specified(None, argLiteral, location = None)
      callArg.children() shouldEqual List(argLiteral)
      val otherLit               = Literal("other_lit", isMethod = false, location = None)
      val callArgWithNewChildren = callArg.withNewChildren(List(otherLit))
      callArgWithNewChildren.children() shouldEqual List(otherLit)
    }

    "work on IR with nested children" in {
      val argLiteral  = Literal("arg", isMethod = false, location = None)
      val callArg     = CallArgument.Specified(None, argLiteral, location = None)
      val funcLiteral = Literal("func", isMethod = false, location = None)
      val app = Application.Prefix(
        funcLiteral,
        List(callArg),
        hasDefaultsSuspended = false,
        location             = None
      )
      app.children() should have size 2
      app.children() shouldEqual List(funcLiteral, callArg)

      val otherArgLiteral =
        Literal("other_arg", isMethod = false, location = None)
      val otherCallArg =
        CallArgument.Specified(None, otherArgLiteral, location = None)
      val otherFuncLiteral =
        Literal("other_func", isMethod = false, location = None)
      val otherApp = app.withNewChildren(List(otherFuncLiteral, otherCallArg))
      otherApp.children() should have size 2
      otherApp.children() shouldEqual List(otherFuncLiteral, otherCallArg)
    }

    "fail if newChildren are given in wrong order" in {
      val argLiteral  = Literal("arg", isMethod = false, location = None)
      val callArg     = CallArgument.Specified(None, argLiteral, location = None)
      val funcLiteral = Literal("func", isMethod = false, location = None)
      val app = Application.Prefix(
        funcLiteral,
        List(callArg),
        hasDefaultsSuspended = false,
        location             = None
      )
      val otherArgLiteral =
        Literal("other_arg", isMethod = false, location = None)
      val otherCallArg =
        CallArgument.Specified(None, otherArgLiteral, location = None)
      val otherFuncLiteral =
        Literal("other_func", isMethod = false, location = None)
      a[IllegalArgumentException] should be thrownBy (
        app.withNewChildren(List(otherCallArg, otherFuncLiteral))
      )
    }

    "binary operator with new children" in {
      implicit val inlineCtx: InlineContext = mkInlineContext
      val ir =
        """
          |1 + 2
          |""".stripMargin.preprocessExpression.get
      ir shouldBe a[Operator.Binary]
      val binOpIr = ir.asInstanceOf[Operator.Binary]
      binOpIr.children should have size 3
      val newBinOp = binOpIr.withNewChildren(
        List(
          binOpIr.left,
          Literal("-", false, None),
          binOpIr.right
        )
      )
      newBinOp.children should have size 3
      newBinOp
        .asInstanceOf[Operator.Binary]
        .operator
        .name shouldEqual "-"
    }
  }
}

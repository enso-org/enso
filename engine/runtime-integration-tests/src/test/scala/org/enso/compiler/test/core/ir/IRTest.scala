package org.enso.compiler.test.core.ir

import org.enso.compiler.core.ir.CallArgument
import org.enso.compiler.core.ir.Name.Literal
import org.enso.compiler.core.ir.expression.Application
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class IRTest extends AnyWordSpecLike with Matchers {
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
  }
}

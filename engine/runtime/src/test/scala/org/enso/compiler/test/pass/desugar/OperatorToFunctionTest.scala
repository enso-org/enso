package org.enso.compiler.test.pass.desugar

import org.enso.compiler.core.ir.{
  CallArgument,
  Empty,
  Expression,
  IdentifiedLocation,
  Location,
  Name
}
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.pass.desugar.OperatorToFunction
import org.enso.compiler.test.CompilerTest

class OperatorToFunctionTest extends CompilerTest {

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

    val leftArg  = CallArgument.Specified(None, left, left.location)
    val rightArg = CallArgument.Specified(None, right, right.location)

    val binOp =
      Operator.Binary(leftArg, name, rightArg, Some(loc))
    val opFn = Application.Prefix(
      name,
      List(leftArg, rightArg),
      hasDefaultsSuspended = false,
      Some(loc)
    )

    (binOp, opFn)
  }

  // === The Tests ============================================================

  "Operators" should {
    val opName =
      Name.Literal("=:=", isMethod = true, None)
    val left     = Empty(None)
    val right    = Empty(None)
    val rightArg = CallArgument.Specified(None, Empty(None), None)

    val (operator, operatorFn) = genOprAndFn(opName, left, right)

    val oprArg   = CallArgument.Specified(None, operator, None)
    val oprFnArg = CallArgument.Specified(None, operatorFn, None)

    "be translated to functions" in {
      OperatorToFunction.runExpression(operator, ctx) shouldEqual operatorFn
    }

//    "be translated in module contexts" in {
//      val moduleInput  = operator.asModuleDefs
//      val moduleOutput = operatorFn.asModuleDefs
//
//      OperatorToFunction.runModule(moduleInput, modCtx) shouldEqual moduleOutput
//    }

    "be translated recursively" in {
      val recursiveIR =
        Operator.Binary(oprArg, opName, rightArg, None)
      val recursiveIRResult = Application.Prefix(
        opName,
        List(oprFnArg, rightArg),
        hasDefaultsSuspended = false,
        None
      )

      OperatorToFunction.runExpression(
        recursiveIR,
        ctx
      ) shouldEqual recursiveIRResult
    }
  }
}

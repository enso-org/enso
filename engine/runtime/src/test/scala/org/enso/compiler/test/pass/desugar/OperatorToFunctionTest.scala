package org.enso.compiler.test.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.IdentifiedLocation
import org.enso.compiler.pass.desugar.OperatorToFunction
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

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
    name: IR.Name,
    left: IR.Expression,
    right: IR.Expression
  ): (IR.Application.Operator.Binary, IR.Application.Prefix) = {
    val loc = IdentifiedLocation(Location(1, 33))

    val leftArg  = IR.CallArgument.Specified(None, left, left.location)
    val rightArg = IR.CallArgument.Specified(None, right, right.location)

    val binOp =
      IR.Application.Operator.Binary(leftArg, name, rightArg, Some(loc))
    val opFn = IR.Application.Prefix(
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
      IR.Name.Literal("=:=", isMethod = true, None)
    val left     = IR.Empty(None)
    val right    = IR.Empty(None)
    val rightArg = IR.CallArgument.Specified(None, IR.Empty(None), None)

    val (operator, operatorFn) = genOprAndFn(opName, left, right)

    val oprArg   = IR.CallArgument.Specified(None, operator, None)
    val oprFnArg = IR.CallArgument.Specified(None, operatorFn, None)

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
        IR.Application.Operator.Binary(oprArg, opName, rightArg, None)
      val recursiveIRResult = IR.Application.Prefix(
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

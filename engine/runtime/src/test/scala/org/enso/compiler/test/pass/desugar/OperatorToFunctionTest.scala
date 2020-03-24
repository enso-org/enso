package org.enso.compiler.test.pass.desugar

import org.enso.compiler.InlineContext
import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.OperatorToFunction
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

class OperatorToFunctionTest extends CompilerTest {

  // === Utilities ============================================================

  val ctx = new InlineContext

  /** Generates an operator and its corresponding function.
    *
    * @param name
    * @param left
    * @param right
    * @return
    */
  def genOprAndFn(
    name: IR.Name,
    left: IR.Expression,
    right: IR.Expression
  ): (IR.Application.Operator.Binary, IR.Application.Prefix) = {
    val loc = Location(1, 33)

    val binOp = IR.Application.Operator.Binary(left, name, right, Some(loc))
    val opFn = IR.Application.Prefix(
      name,
      List(
        IR.CallArgument.Specified(None, left, left.location),
        IR.CallArgument.Specified(None, right, right.location)
      ),
      hasDefaultsSuspended = false,
      Some(loc)
    )

    (binOp, opFn)
  }

  // === The Tests ============================================================

  "Operators" should {
    val opName = IR.Name.Literal("=:=", None)
    val left   = IR.Empty(None)
    val right  = IR.Empty(None)

    val (operator, operatorFn) = genOprAndFn(opName, left, right)

    "be translated to functions" in {
      OperatorToFunction.runExpression(operator, ctx) shouldEqual operatorFn
    }

    "be translated in module contexts" in {
      val moduleInput  = operator.asModuleDefs
      val moduleOutput = operatorFn.asModuleDefs

      OperatorToFunction.runModule(moduleInput) shouldEqual moduleOutput
    }

    "be translated recursively" in {
      val recursiveIR =
        IR.Application.Operator.Binary(operator, opName, right, None)
      val recursiveIRResult = IR.Application.Prefix(
        opName,
        List(
          IR.CallArgument.Specified(None, operatorFn, operatorFn.location),
          IR.CallArgument.Specified(None, right, right.location)
        ),
        hasDefaultsSuspended = false,
        None
      )

      OperatorToFunction.runExpression(recursiveIR, ctx) shouldEqual recursiveIRResult
    }
  }
}

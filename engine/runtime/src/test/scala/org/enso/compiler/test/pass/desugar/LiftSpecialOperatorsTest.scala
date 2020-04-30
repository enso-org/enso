package org.enso.compiler.test.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.IdentifiedLocation
import org.enso.compiler.pass.desugar.LiftSpecialOperators
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

class LiftSpecialOperatorsTest extends CompilerTest {

  // === Utilities ============================================================

  val ctx    = InlineContext()
  val modCtx = ModuleContext()

  /** Tests whether a given operator is lifted correctly into the corresponding
    * special construct.
    *
    * @param opInfo the operator symbol
    * @param constructor the way to construct the operator
    */
  def testOperator(
    opInfo: IR.Type.Info,
    constructor: (
      IR.Expression,
      IR.Expression,
      Option[IdentifiedLocation]
    ) => IR.Expression
  ): Unit = s"The ${opInfo.name} operator" should {
    val op    = IR.Name.Literal(opInfo.name, None)
    val left  = IR.Empty(None)
    val right = IR.Empty(None)
    val loc   = IdentifiedLocation(Location(1, 20))

    val expressionIR = IR.Application.Operator.Binary(
      left,
      op,
      right,
      Some(loc)
    )
    val outputExpressionIR = constructor(
      left,
      right,
      Some(loc)
    )

    "be lifted by the pass in an inline context" in {
      LiftSpecialOperators
        .runExpression(expressionIR, ctx) shouldEqual outputExpressionIR
    }

    "be lifted by the pass in a module context" in {
      val moduleInput  = expressionIR.asModuleDefs
      val moduleOutput = outputExpressionIR.asModuleDefs

      LiftSpecialOperators
        .runModule(moduleInput, modCtx) shouldEqual moduleOutput
    }

    "work recursively where necessary" in {
      val recursiveIR =
        IR.Application.Operator.Binary(expressionIR, op, right, None)
      val recursiveIROutput = constructor(
        constructor(left, right, Some(loc)),
        right,
        None
      )

      LiftSpecialOperators
        .runExpression(recursiveIR, ctx) shouldEqual recursiveIROutput
    }
  }

  // === The Tests ============================================================

  testOperator(IR.Type.Ascription, IR.Type.Ascription(_, _, _))

  testOperator(
    IR.Type.Set.Subsumption,
    IR.Type.Set.Subsumption(_, _, _)
  )

  testOperator(IR.Type.Set.Equality, IR.Type.Set.Equality(_, _, _))

  testOperator(IR.Type.Set.Concat, IR.Type.Set.Concat(_, _, _))

  testOperator(IR.Type.Set.Union, IR.Type.Set.Union(_, _, _))

  testOperator(IR.Type.Set.Intersection, IR.Type.Set.Intersection(_, _, _))

  testOperator(IR.Type.Set.Subtraction, IR.Type.Set.Subtraction(_, _, _))
}

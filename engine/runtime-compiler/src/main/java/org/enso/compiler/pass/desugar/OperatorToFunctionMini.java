package org.enso.compiler.pass.desugar;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Operator;
import org.enso.compiler.pass.MiniIRPass;
import scala.collection.mutable.ListBuffer;

public class OperatorToFunctionMini extends MiniIRPass {
  OperatorToFunctionMini() {}

  @Override
  public Expression transformExpression(Expression ir) {
    if (ir instanceof Operator.Binary binOp) {
      ListBuffer<CallArgument> args = new ListBuffer<>();
      args.addOne(binOp.left());
      args.addOne(binOp.right());
      return new Application.Prefix(
          binOp.operator(),
          args.toList(),
          false,
          binOp.location().isDefined() ? binOp.location().get() : null,
          binOp.passData(),
          binOp.diagnostics());
    }
    return ir;
  }

  @Override
  public boolean checkPostCondition(IR ir) {
    boolean[] isChildOperator = {false};
    ir.children()
        .foreach(
            child -> {
              if (child instanceof Operator.Binary) {
                isChildOperator[0] = true;
              }
              return null;
            });
    return !isChildOperator[0];
  }

  /* from develop:
  ): Expression =
    ir.transformExpressions { case operatorBinary: Operator.Binary =>
      new Application.Prefix(
        operatorBinary.operator,
        List(
          operatorBinary.left.mapExpressions(runExpression(_, inlineContext)),
          operatorBinary.right.mapExpressions(runExpression(_, inlineContext))
        ),
        hasDefaultsSuspended = false,
        operatorBinary.identifiedLocation,
        operatorBinary.passData,
        operatorBinary.diagnostics
      )
    }
  */
}

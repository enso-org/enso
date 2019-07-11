package org.enso.interpreter.node.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

@NodeInfo(shortName = "{}")
public class FunctionBodyNode extends ExpressionNode {

  @Children private final ExpressionNode[] statements;
  @Child private ExpressionNode returnExpr;

  public FunctionBodyNode(ExpressionNode[] statements, ExpressionNode returnExpr) {
    this.statements = statements;
    this.returnExpr = returnExpr;
    returnExpr.markTail();
  }

  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    for (ExpressionNode statement : statements) {
      statement.executeVoid(frame);
    }
    return returnExpr.executeGeneric(frame);
  }
}

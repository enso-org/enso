package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/**
 * This node defines the body of a function for execution, as well as the protocol for executing the
 * function body.
 */
@NodeInfo(shortName = "{}")
public class FunctionBodyNode extends ExpressionNode {
  @Children private final ExpressionNode[] statements;
  @Child private ExpressionNode returnExpr;

  /**
   * Creates a new node to represent the body of a function.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   */
  public FunctionBodyNode(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    this.statements = expressions;
    this.returnExpr = returnExpr;
  }

  /**
   * Sets whether or not the function is tail-recursive.
   *
   * @param isTail whether or not the function is tail-recursive.
   */
  @Override
  public void setTail(boolean isTail) {
    returnExpr.setTail(isTail);
  }

  /**
   * Executes the body of the function.
   *
   * @param frame the stack frame for execution
   * @return the result of executing this function
   */
  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    for (ExpressionNode statement : statements) {
      statement.executeVoid(frame);
    }
    return returnExpr.executeGeneric(frame);
  }
}

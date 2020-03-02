package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/**
 * This node defines the body of a function for execution, as well as the protocol for executing the
 * function body.
 */
@NodeInfo(shortName = "Block")
public class BlockNode extends ExpressionNode {
  @Children private final ExpressionNode[] statements;
  @Child private ExpressionNode returnExpr;

  private BlockNode(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    this.statements = expressions;
    this.returnExpr = returnExpr;
  }

  /**
   * Creates an instance of this node.
   *
   * @param expressions the function body
   * @param returnExpr the return expression from the function
   * @return a node representing a block expression
   */
  public static BlockNode build(ExpressionNode[] expressions, ExpressionNode returnExpr) {
    return new BlockNode(expressions, returnExpr);
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

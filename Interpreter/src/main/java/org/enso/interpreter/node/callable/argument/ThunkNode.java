package org.enso.interpreter.node.callable.argument;

import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;

/**
 * Node acting as root for argument thunks. Executes its child by passing it a definition-site
 * stack frame obtained from arguments.
 */
public class ThunkNode extends ExpressionNode {
  @Child private ExpressionNode exprNode;

  /**
   * Creates an instance of this node.
   *
   * @param exprNode the child node representing the suspended computation.
   */
  public ThunkNode(ExpressionNode exprNode) {
    this.exprNode = exprNode;
  }

  /**
   * Executes the child expression by passing it the definition-site frame obtained from arguments.
   *
   * @param frame the stack frame for execution
   * @return the result of executing the child node in the proper stack frame
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return exprNode.executeGeneric((MaterializedFrame) frame.getArguments()[0]);
  }
}

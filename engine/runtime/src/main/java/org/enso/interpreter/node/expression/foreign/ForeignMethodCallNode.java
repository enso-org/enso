package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.node.ExpressionNode;

/** Performs a call into a given foreign call target. */
public class ForeignMethodCallNode extends ExpressionNode {
  private @Children ExpressionNode[] arguments;
  private @Child DirectCallNode callNode;

  ForeignMethodCallNode(ExpressionNode[] arguments, CallTarget foreignCt) {
    this.arguments = arguments;
    this.callNode = DirectCallNode.create(foreignCt);
  }

  /**
   * Creates a new instance of this node
   *
   * @param arguments expressions resulting in the computation of function arguments
   * @param foreignCt the foreign call target to call
   * @return the result of calling the foreign call target with the executed arguments
   */
  public static ForeignMethodCallNode build(ExpressionNode[] arguments, CallTarget foreignCt) {
    return new ForeignMethodCallNode(arguments, foreignCt);
  }

  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Object[] args = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      args[i] = arguments[i].executeGeneric(frame);
    }
    return callNode.call(args);
  }
}

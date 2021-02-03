package org.enso.interpreter.node.expression.foreign;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.node.ExpressionNode;

import java.util.Arrays;

public class ForeignMethodCallNode extends ExpressionNode {
  private @Children ExpressionNode[] arguments;
  private @Child DirectCallNode callNode;

  public ForeignMethodCallNode(ExpressionNode[] arguments, CallTarget foreignCt) {
    this.arguments = arguments;
    this.callNode = DirectCallNode.create(foreignCt);
  }

  public static ForeignMethodCallNode create(ExpressionNode[] arguments, CallTarget foreignCt) {
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

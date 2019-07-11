package org.enso.interpreter.node.function;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Function;
import org.enso.interpreter.optimiser.TailCallException;

@NodeInfo(shortName = "@", description = "Executes function")
public final class InvokeNode extends ExpressionNode {
  @Children private final ExpressionNode[] arguments;
  @Child private ExpressionNode expression;
  @Child private DispatchNode dispatchNode;

  public InvokeNode(ExpressionNode expression, ExpressionNode[] arguments) {
    this.expression = expression;
    this.arguments = arguments;
    this.dispatchNode = new SimpleDispatchNode();
  }

  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Function function = (Function) expression.executeGeneric(frame);
    Object[] positionalArguments = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      positionalArguments[i] = arguments[i].executeGeneric(frame);
    }

    CompilerAsserts.compilationConstant(this.isTail());
    if (this.isTail()) {
      throw new TailCallException(function, positionalArguments);
    } else {
      return dispatchNode.executeDispatch(function, positionalArguments);
    }
  }
}

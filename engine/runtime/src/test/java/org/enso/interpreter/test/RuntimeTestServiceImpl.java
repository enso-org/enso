package org.enso.interpreter.test;

import java.util.UUID;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.test.instruments.RuntimeTestService;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(
    service = RuntimeTestService.class
)
public class RuntimeTestServiceImpl implements RuntimeTestService {

  @Override
  public UUID getExpressionNodeID(Object expressionNode) {
    if (expressionNode instanceof ExpressionNode exprNode) {
      return exprNode.getId();
    } else {
      throw new IllegalArgumentException("Expected ExpressionNode, got " + expressionNode);
    }
  }

  @Override
  public boolean isExpressionNode(Object node) {
    return node instanceof ExpressionNode;
  }

  @Override
  public boolean isTailCallException(Object obj) {
    return obj instanceof TailCallException;
  }
}

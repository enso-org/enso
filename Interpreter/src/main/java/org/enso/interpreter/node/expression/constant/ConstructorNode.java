package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.AtomConstructor;

public class ConstructorNode extends ExpressionNode {
  private final AtomConstructor constructor;

  public ConstructorNode(AtomConstructor constructor) {
    this.constructor = constructor;
  }

  @Override
  public AtomConstructor executeAtomConstructor(VirtualFrame frame) {
    return constructor;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return constructor;
  }
}

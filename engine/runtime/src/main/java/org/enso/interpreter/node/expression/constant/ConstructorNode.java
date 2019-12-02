package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** Represents a type constructor definition. */
public class ConstructorNode extends ExpressionNode {
  private final AtomConstructor constructor;

  /**
   * Creates a new type constructor definition.
   *
   * @param constructor the constructor to define
   */
  public ConstructorNode(AtomConstructor constructor) {
    this.constructor = constructor;
  }

  /**
   * Executes the type constructor definition.
   *
   * @param frame the frame to execute in
   * @return the constructor of the type defined
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (constructor.getArity() == 0) {
      return constructor.newInstance();
    } else {
      return constructor;
    }
  }
}

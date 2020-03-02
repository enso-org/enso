package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** Represents a type constructor definition. */
@NodeInfo(shortName = "Cons", description = "Represents a constructor definition")
public class ConstructorNode extends ExpressionNode {
  private final AtomConstructor constructor;

  private ConstructorNode(AtomConstructor constructor) {
    this.constructor = constructor;
  }

  /**
   * Creates an instance of this node.
   *
   * @param constructor the atom constructor to represent
   * @return a truffle node representing {@code constructor}
   */
  public static ConstructorNode build(AtomConstructor constructor) {
    return new ConstructorNode(constructor);
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

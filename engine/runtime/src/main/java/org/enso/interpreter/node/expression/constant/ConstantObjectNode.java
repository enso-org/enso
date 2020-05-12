package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/** Represents a compile-time constant. */
@NodeInfo(shortName = "const", description = "Represents an arbitrary compile-time constant.")
public class ConstantObjectNode extends ExpressionNode {
  private final Object object;

  private ConstantObjectNode(Object object) {
    this.object = object;
  }

  /**
   * Creates an instance of this node.
   *
   * @param object the constant to represent
   * @return a truffle node representing {@code constructor}
   */
  public static ConstantObjectNode build(Object object) {
    return new ConstantObjectNode(object);
  }

  /**
   * Executes the type constructor definition.
   *
   * @param frame the frame to execute in
   * @return the constructor of the type defined
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return object;
  }
}

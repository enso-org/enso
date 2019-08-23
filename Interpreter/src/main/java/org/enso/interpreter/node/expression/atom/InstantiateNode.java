package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/**
 * A node instantiating a constant {@link AtomConstructor} with values computed based on the
 * children nodes.
 */
public class InstantiateNode extends ExpressionNode {
  private final AtomConstructor constructor;
  private @Children ExpressionNode[] arguments;

  /**
   * Creates a new {@link InstantiateNode}.
   *
   * @param constructor the {@link AtomConstructor} this node will be instantiating
   * @param arguments the expressions for field values
   */
  public InstantiateNode(AtomConstructor constructor, ExpressionNode[] arguments) {
    this.constructor = constructor;
    this.arguments = arguments;
  }

  /**
   * Executes the node, by executing all its children and putting their values as fields of the
   * newly created {@link AtomConstructor} instance.
   *
   * @param frame the stack frame for execution
   * @return the newly created {@link AtomConstructor} instance.
   */
  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Object[] argumentValues = new Object[arguments.length];
    for (int i = 0; i < arguments.length; i++) {
      argumentValues[i] = arguments[i].executeGeneric(frame);
    }
    return constructor.newInstance(argumentValues);
  }
}

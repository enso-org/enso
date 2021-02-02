package org.enso.interpreter.node.expression.atom;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;

/**
 * A node instantiating a constant {@link AtomConstructor} with values computed based on the
 * children nodes.
 */
@NodeInfo(shortName = "Instantiate", description = "Instantiates a constant Atom constructor")
public class InstantiateNode extends ExpressionNode {
  private final AtomConstructor constructor;
  private @Children ExpressionNode[] arguments;

  InstantiateNode(AtomConstructor constructor, ExpressionNode[] arguments) {
    this.constructor = constructor;
    this.arguments = arguments;
  }

  /**
   * Creates an instance of this node.
   *
   * @param constructor the {@link AtomConstructor} this node will be instantiating
   * @param arguments the expressions for field values
   * @return a node that instantiates {@code constructor}
   */
  public static InstantiateNode build(AtomConstructor constructor, ExpressionNode[] arguments) {
    return new InstantiateNode(constructor, arguments);
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

package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;

@NodeInfo(shortName = "[]", description = "Creates a vector from given expressions.")
public class SequenceLiteralNode extends ExpressionNode {
  private @Children ExpressionNode[] items;

  private SequenceLiteralNode(ExpressionNode[] items) {
    this.items = items;
  }

  /**
   * Creates a new instance of this node.
   *
   * @param items the expressions evaluating to the vector elements.
   * @return a new instance of this node.
   */
  public static SequenceLiteralNode build(ExpressionNode[] items) {
    return new SequenceLiteralNode(items);
  }

  /**
   * Executes the node.
   *
   * @param frame the stack frame for execution.
   * @return a {@link Array} containing the results of evaluating child expressions.
   */
  @Override
  @ExplodeLoop
  public Object executeGeneric(VirtualFrame frame) {
    Object[] itemValues = new Object[items.length];
    for (int i = 0; i < items.length; i++) {
      itemValues[i] = items[i].executeGeneric(frame);
    }
    return Vector.fromArray(new Array(itemValues));
  }
}

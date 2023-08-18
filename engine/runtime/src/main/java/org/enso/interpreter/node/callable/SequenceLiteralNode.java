package org.enso.interpreter.node.callable;

import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicSentinel;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;

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
      if (itemValues[i] instanceof PanicSentinel sentinel) {
        throw sentinel;
      }
    }
    return ArrayLikeHelpers.asVectorWithCheckAt(itemValues);
  }
}

package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** The subtraction operator for Enso. */
@NodeInfo(shortName = "-")
public abstract class SubtractOperatorNode extends BinaryOperatorNode {

  SubtractOperatorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param left the left operand
   * @param right the right operand
   * @return a node that subtracts {@code right} from {@code left}
   */
  public static SubtractOperatorNode build(ExpressionNode left, ExpressionNode right) {
    return SubtractOperatorNodeGen.create(left, right);
  }

  /**
   * Subtracts one number from another.
   *
   * @param left the minuend
   * @param right the subtrahend
   * @return the result of subtracting {@code right} from {@code left}
   */
  @Specialization
  protected long subtract(long left, long right) {
    return left - right;
  }
}

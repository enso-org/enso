package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

/**
 * The subtraction operator for Enso.
 */
@NodeInfo(shortName = "-")
public abstract class SubtractOperatorNode extends BinaryOperatorNode {

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

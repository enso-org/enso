package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

/** The division operator for Enso. */
@NodeInfo(shortName = "/")
public abstract class DivideOperatorNode extends BinaryOperatorNode {

  /**
   * Performs integer division of two numbers.
   *
   * @param left the dividend
   * @param right the divisor
   * @return the result of dividing {@code left} by {@code right}
   */
  @Specialization
  protected long divide(long left, long right) {
    return left / right;
  }
}

package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

/**
 * The multiplication operator for Enso.
 */
@NodeInfo(shortName = "*")
public abstract class MultiplyOperatorNode extends BinaryOperatorNode {

  /**
   * Multiplies two numbers together.
   *
   * @param left the first factor
   * @param right the second factor
   * @return the result of {@code left} multiplied by {@code right}
   */
  @Specialization
  protected long multiply(long left, long right) {
    return left * right;
  }
}

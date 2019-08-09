package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

/**
 * The modulo operator for Enso.
 */
@NodeInfo(shortName = "%")
public abstract class ModOperatorNode extends BinaryOperatorNode {

  /**
   * Calculates the modulus of two numbers.
   *
   * @param left the dividend
   * @param right the divisor
   * @return the result of calculating {@code left} modulo {@code right}
   */
  @Specialization
  protected long mod(long left, long right) {
    return left % right;
  }
}

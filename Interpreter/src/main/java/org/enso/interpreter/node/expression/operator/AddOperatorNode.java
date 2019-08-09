package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;

/**
 * The addition operator for Enso.
 */
@NodeInfo(shortName = "+")
public abstract class AddOperatorNode extends BinaryOperatorNode {

  /**
   * Adds two numbers together.
   *
   * @param left the first summand
   * @param right the second summand
   * @return the result of adding {@code left} and {@code right}
   */
  @Specialization
  protected long add(long left, long right) {
    return left + right;
  }
}

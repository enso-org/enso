package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** The modulo operator for Enso. */
@NodeInfo(shortName = "%")
public abstract class ModOperatorNode extends BinaryOperatorNode {

  ModOperatorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param left the left operand
   * @param right the right operand
   * @return a node that calculates the remainder when {@code left} is divided by {@code right}
   */
  public static ModOperatorNode build(ExpressionNode left, ExpressionNode right) {
    return ModOperatorNodeGen.create(left, right);
  }

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

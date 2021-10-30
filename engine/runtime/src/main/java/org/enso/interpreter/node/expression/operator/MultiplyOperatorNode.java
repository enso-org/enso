package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** The multiplication operator for Enso. */
@NodeInfo(shortName = "*")
public abstract class MultiplyOperatorNode extends BinaryOperatorNode {

  MultiplyOperatorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param left the left operand
   * @param right the right operand
   * @return a node that multiplies {@code left} by {@code right}
   */
  public static MultiplyOperatorNode build(ExpressionNode left, ExpressionNode right) {
    return MultiplyOperatorNodeGen.create(left, right);
  }

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

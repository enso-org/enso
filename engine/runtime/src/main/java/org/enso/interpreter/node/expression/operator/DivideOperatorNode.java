package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** The division operator for Enso. */
@NodeInfo(shortName = "/")
public abstract class DivideOperatorNode extends BinaryOperatorNode {

  DivideOperatorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param left the left operand
   * @param right the right operand
   * @return a node that divides {@code left} by {@code right}
   */
  public static DivideOperatorNode build(ExpressionNode left, ExpressionNode right) {
    return DivideOperatorNodeGen.create(left, right);
  }

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

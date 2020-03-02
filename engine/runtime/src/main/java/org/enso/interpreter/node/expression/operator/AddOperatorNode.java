package org.enso.interpreter.node.expression.operator;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** The addition operator for Enso. */
@NodeInfo(shortName = "+")
public abstract class AddOperatorNode extends BinaryOperatorNode {

  AddOperatorNode() {}

  /**
   * Creates an instance of this node.
   *
   * @param left the left operand
   * @param right the right operand
   * @return a node that adds {@code left} to {@code right}
   */
  public static AddOperatorNode build(ExpressionNode left, ExpressionNode right) {
    return AddOperatorNodeGen.create(left, right);
  }

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

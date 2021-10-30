package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** A representation of decimal literals in Enso. */
@NodeInfo(description = "A decimal literal.")
public final class DecimalLiteralNode extends ExpressionNode {
  private final double value;

  private DecimalLiteralNode(double value) {
    this.value = value;
  }

  /**
   * Creates an instance of this node.
   *
   * @param value the value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static DecimalLiteralNode build(double value) {
    return new DecimalLiteralNode(value);
  }

  /**
   * Gets the value of the literal.
   *
   * @param frame the stack frame for execution
   * @return the value of the integer literal
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return this.value;
  }
}

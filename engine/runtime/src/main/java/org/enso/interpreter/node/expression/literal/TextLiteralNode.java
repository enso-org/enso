package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** Node representing a constant String value. */
@NodeInfo(shortName = "StringLiteral", description = "Constant string literal expression")
public class TextLiteralNode extends ExpressionNode {
  private final String value;

  /**
   * Creates a new instance of this node.
   *
   * @param value the literal value this node represents
   */
  public TextLiteralNode(String value) {
    this.value = value;
  }

  /**
   * Returns the constant value of this string literal.
   *
   * @param frame the stack frame for execution
   * @return the string value this node was created with
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return value;
  }
}

package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;

/** A representation of integer literals in Enso. */
@NodeInfo(shortName = "IntegerLiteral")
public final class IntegerLiteralNode extends LiteralNode {
  private final long value;

  /**
   * Creates a new integer literal.
   *
   * @param value the integer value of the literal
   */
  public IntegerLiteralNode(long value) {
    this.value = value;
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

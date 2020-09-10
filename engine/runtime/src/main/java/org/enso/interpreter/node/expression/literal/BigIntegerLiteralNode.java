package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

import java.math.BigInteger;

/** A representation of big integer literals in Enso. */
@NodeInfo(shortName = "BigIntegerLiteral")
public final class BigIntegerLiteralNode extends ExpressionNode {
  private final EnsoBigInteger value;

  private BigIntegerLiteralNode(BigInteger value) {
    this.value = new EnsoBigInteger(value);
  }

  /**
   * Creates an instance of this node.
   *
   * @param value the value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static BigIntegerLiteralNode build(BigInteger value) {
    return new BigIntegerLiteralNode(value);
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

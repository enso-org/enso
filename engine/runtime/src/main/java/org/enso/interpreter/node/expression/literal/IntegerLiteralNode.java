package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;

/** A representation of integer literals in Enso. */
@NodeInfo(shortName = "IntegerLiteral")
public final class IntegerLiteralNode extends ExpressionNode {
  private static final Assumption CONSTANTS_ARE_CONSTANTS =
      Truffle.getRuntime().createAssumption("Constants were never updated");
  @CompilerDirectives.CompilationFinal private long value;

  private IntegerLiteralNode(long value) {
    this.value = value;
  }

  /**
   * Creates an instance of this node.
   *
   * @param value the value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static IntegerLiteralNode build(long value) {
    return new IntegerLiteralNode(value);
  }

  /**
   * Gets the value of the literal.
   *
   * @param frame the stack frame for execution
   * @return the value of the integer literal
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (CONSTANTS_ARE_CONSTANTS.isValid()) {
      return this.value;
    }
    return readValue();
  }

  @CompilerDirectives.TruffleBoundary
  private Object readValue() {
    return this.value;
  }

  public void updateConstant(String text) {
    this.value = Long.valueOf(text);
    CONSTANTS_ARE_CONSTANTS.invalidate();
  }
}

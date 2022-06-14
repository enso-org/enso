package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Literal$Number;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.tag.Patchable;

/** A representation of integer literals in Enso. */
@NodeInfo(shortName = "IntegerLiteral")
public final class IntegerLiteralNode extends ExpressionNode implements Patchable {
  private final long value;

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
    return this.value;
  }

  @Override
  public Object parsePatch(IR.Expression ir) {
    if (ir instanceof IR$Literal$Number n && n.numericValue() instanceof Long l) {
      return l;
    } else {
      return null;
    }
  }
}

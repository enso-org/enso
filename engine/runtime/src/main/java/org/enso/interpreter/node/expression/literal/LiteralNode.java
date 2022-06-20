package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.math.BigInteger;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Literal$Number;
import org.enso.compiler.core.IR$Literal$Text;
import org.enso.compiler.exception.CompilerError;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.tag.Patchable;

/** Generic literal node. */
@NodeInfo(shortName = "Literal", description = "Constant literal expression")
public class LiteralNode extends ExpressionNode implements Patchable {
  private final Object value;

  private LiteralNode(Object value) {
    this.value = value;
  }

  /**
   * Creates an instance of literal node.
   *
   * @param value the textual value to represent
   * @return a node representing the literal {@code value}
   */
  public static LiteralNode build(String value) {
    return new LiteralNode(Text.create(value));
  }

  /**
   * Creates an instance of the literal node.
   *
   * @param value the value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static LiteralNode build(long value) {
    return new LiteralNode(value);
  }

  /**
   * Creates an instance of the literal node.
   *
   * @param value decimal value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static LiteralNode build(double value) {
    return new LiteralNode(value);
  }

  /**
   * Creates an instance of the literal node.
   *
   * @param value big integer value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static LiteralNode build(BigInteger value) {
    return new LiteralNode(new EnsoBigInteger(value));
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

  @Override
  public Runnable parsePatch(IR.Expression ir) {
    Object newValue = parseLiteralIr(ir);
    if (newValue != null) {
      return () -> {
        replace(PatchableLiteralNode.build(newValue));
      };
    } else {
      return null;
    }
  }

  static Object parseLiteralIr(IR.Expression ir) throws CompilerError {
    return switch (ir) {
      case IR$Literal$Text t -> Text.create(t.text());
      case IR$Literal$Number n -> n.numericValue();
      default -> null;
    };
  }
}

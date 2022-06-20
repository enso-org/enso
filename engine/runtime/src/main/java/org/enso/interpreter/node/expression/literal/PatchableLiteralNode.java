package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.math.BigInteger;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Literal$Number;
import org.enso.compiler.core.IR$Literal$Text;
import org.enso.interpreter.node.ExpressionNode;
import static org.enso.interpreter.node.expression.literal.LiteralNode.parseLiteralIr;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.tag.Patchable;

/** Generic literal node. */
@NodeInfo(shortName = "Literal", description = "Constant literal expression")
final class PatchableLiteralNode extends ExpressionNode implements Patchable {
  private Object value;

  private PatchableLiteralNode(Object value) {
    this.value = value;
  }

  static PatchableLiteralNode build(Object value) {
    return new PatchableLiteralNode(value);
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
    var newValue = parseLiteralIr(ir);
    if (newValue != null) {
      return () -> {
        this.value = newValue;
      };
    } else {
      return null;
    }
  }
}

package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.function.Predicate;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Literal;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.tag.Patchable;

/** Generic literal node. */
@NodeInfo(shortName = "Literal", description = "Constant literal expression")
final class PatchableLiteralNode extends ExpressionNode implements Patchable, Predicate<Expression> {
  private final LiteralNode node;
  private Object value;

  private PatchableLiteralNode(LiteralNode original) {
    this.node = original;
    this.value = original.executeGeneric(null);
    this.setId(original.getId());
  }

  static PatchableLiteralNode build(LiteralNode original) {
    return new PatchableLiteralNode(original);
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
  public boolean test(Expression ir) {
    var newValue = parseLiteralIr(ir);
    if (newValue != null && this.value.getClass() == newValue.getClass()) {
      this.value = newValue;
      return true;
    } else {
      return false;
    }
  }

  @Override
  public int[] getSourceSectionBounds() {
    return node.getSourceSectionBounds();
  }

  @Override
  public boolean hasTag(Class<? extends com.oracle.truffle.api.instrumentation.Tag> tag) {
    return Patchable.Tag.class == tag || super.hasTag(tag);
  }

  @Override
  @SuppressWarnings("unchecked")
  public <N extends Node & Predicate<Expression>> N asPatchableNode() {
    return (N) this;
  }

  private static Object parseLiteralIr(Expression ir) {
    return switch (ir) {
      case Literal.Text t -> Text.create(t.text());
      case Literal.Number n -> n.numericValue();
      default -> null;
    };
  }

}

package org.enso.interpreter.node.expression.literal;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.math.BigInteger;
import java.util.function.Predicate;
import org.enso.compiler.core.ir.Expression;
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
   * Creates an instance of the literal node.
   *
   * @param value interop value for the node to represent
   * @return a node representing the literal given by {@code value}
   */
  public static LiteralNode build(TruffleObject value) {
    return new LiteralNode(value);
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
  @SuppressWarnings("unchecked")
  public <N extends Node & Predicate<Expression>> N asPatchableNode() {
    var p = PatchableLiteralNode.build(this);
    notifyInserted(replace(p));
    return (N) p;
  }

  @Override
  public boolean hasTag(Class<? extends com.oracle.truffle.api.instrumentation.Tag> tag) {
    return Patchable.Tag.class == tag || super.hasTag(tag);
  }
}

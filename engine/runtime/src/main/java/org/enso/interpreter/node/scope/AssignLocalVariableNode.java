package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.polyglot.RuntimeID;

/**
 * Tracks local variable assignment for instrumentation. Used by {@link AssignmentNode} when {@link
 * StandardTags.WriteVariableTag} instrumentation is requested. By having a dedicated node the
 * instrumentation can check the "on return value" that's being assigned to the local variable.
 * That's contrary to {@link AssignmentNode} which always returns {@code Nothing}.
 */
final class AssignLocalVariableNode extends ExpressionNode {
  private @CompilerDirectives.CompilationFinal RuntimeID id = null;
  private final String name;
  @Child private ExpressionNode rhs;

  AssignLocalVariableNode(String name, ExpressionNode expression) {
    this.name = name;
    this.rhs = expression;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return rhs.executeGeneric(frame);
  }

  @Override
  public Object getNodeObject() {
    return new VariableNodeObject(StandardTags.WriteVariableTag.NAME, name);
  }

  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    if (super.hasTag(tag)) {
      return true;
    } else {
      assert getSourceSectionBounds() != null;
      return StandardTags.WriteVariableTag.class == tag;
    }
  }

  @Override
  public RuntimeID getId() {
    return this.id;
  }

  @Override
  public void setId(RuntimeID id) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.id = id;
  }
}

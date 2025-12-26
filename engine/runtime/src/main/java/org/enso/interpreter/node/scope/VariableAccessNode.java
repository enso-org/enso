package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import org.enso.interpreter.node.ExpressionNode;

final class VariableAccessNode extends ExpressionNode {
  private final String name;
  @Child private ExpressionNode rhs;

  VariableAccessNode(String name, ExpressionNode expression) {
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
}

package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;

/**
 * Node tagged with {@link StandardTags.StatementTag}. Inserted by {@link BlockNode} into the AST
 * when debugger is connected.
 */
final class StatementNode extends ExpressionNode {
  @Child ExpressionNode node;

  private StatementNode(ExpressionNode node) {
    this.node = node;
  }

  static StatementNode wrap(ExpressionNode node) {
    if (node instanceof StatementNode statement) {
      return statement;
    } else {
      return new StatementNode(node);
    }
  }

  @Override
  public SourceSection getSourceSection() {
    return node.getSourceSection();
  }

  @Override
  public boolean isInstrumentable() {
    return getSourceSection() != null && node.isInstrumentable();
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return node.executeGeneric(frame);
  }

  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    if (AvoidIdInstrumentationTag.class == tag) {
      return getRootNode() instanceof ClosureRootNode c && !c.isSubjectToInstrumentation();
    }
    return StandardTags.StatementTag.class == tag;
  }
}

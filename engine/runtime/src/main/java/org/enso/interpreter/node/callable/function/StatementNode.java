package org.enso.interpreter.node.callable.function;

import com.oracle.truffle.api.frame.Frame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.interop.NodeLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.scope.DebugLocalScope;

/**
 * Node tagged with {@link StandardTags.StatementTag}. Inserted by {@link BlockNode} into the AST
 * when debugger is connected.
 */
@ExportLibrary(NodeLibrary.class)
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
    return StandardTags.StatementTag.class == tag;
  }

  @ExportMessage
  boolean hasScope(Frame frame) {
    return true;
  }

  @ExportMessage
  Object getScope(Frame frame, boolean nodeEnter) {
    RootNode rootNode = getRootNode();
    if (rootNode == null) {
      return null;
    } else {
      return DebugLocalScope.createFromFrame((EnsoRootNode) rootNode, frame.materialize());
    }
  }
}

package org.enso.interpreter.node;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.Language;

public class EnsoRootNode extends RootNode {
  @Child private ExpressionNode body;

  private final String name;
  private final SourceSection sourceSection;

  public EnsoRootNode(
      Language language,
      FrameDescriptor frameDescriptor,
      ExpressionNode body,
      SourceSection section,
      String name) {
    super(language, frameDescriptor);
    this.body = body;
    this.sourceSection = section;
    this.name = name;
  }

  @Override
  public Object execute(VirtualFrame frame) {
    return body.executeGeneric(frame);
  }

  @Override
  public String toString() {
    return this.name;
  }
}

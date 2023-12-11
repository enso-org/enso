package org.enso.interpreter.arrow.node;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.arrow.ArrowLanguage;
import org.enso.interpreter.arrow.ArrowParser;

public class ArrowEvalNode extends RootNode {
  private final ArrowParser.Result code;

  @Child private ArrowFixedSizeNode fixedPhysicalLayout = ArrowFixedSizeNode.build();

  public static ArrowEvalNode build(ArrowLanguage language, ArrowParser.Result code) {
    return new ArrowEvalNode(language, code);
  }

  private ArrowEvalNode(ArrowLanguage language, ArrowParser.Result code) {
    super(language, new FrameDescriptor());
    this.code = code;
  }

  public Object execute(VirtualFrame frame) {
    return fixedPhysicalLayout.execute(code.getLogicalLayout());
  }
}

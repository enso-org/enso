package org.enso.interpreter.node.function;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Function;

@NodeInfo(description = "Read function argument.")
public class ReadArgumentNode extends ExpressionNode {
  private final int index;

  public ReadArgumentNode(int index) {
    this.index = index;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return Function.ArgumentsHelper.getPositionalArguments(frame.getArguments())[index];
  }
}

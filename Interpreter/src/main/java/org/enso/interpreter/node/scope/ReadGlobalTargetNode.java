package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.GlobalCallTarget;

@NodeInfo(shortName = "Global Call Target")
public class ReadGlobalTargetNode extends ExpressionNode {

  private final GlobalCallTarget globalCallTarget;
  @Child private DirectCallNode directCallNode = null;

  public ReadGlobalTargetNode(GlobalCallTarget globalCallTarget) {
    this.globalCallTarget = globalCallTarget;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (directCallNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();

      directCallNode = Truffle.getRuntime().createDirectCallNode(globalCallTarget.getTarget());
    }

    return directCallNode.call();
  }
}

package org.enso.interpreter.node.scope;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.GlobalCallTarget;

/** A representation of a read from a global call target. */
@NodeInfo(shortName = "Global Call Target")
public class ReadGlobalTargetNode extends ExpressionNode {
  private final GlobalCallTarget globalCallTarget;
  @Child private DirectCallNode directCallNode = null;

  /**
   * Creates a new node to read from a global call target.
   *
   * @param globalCallTarget the call target to read from
   */
  public ReadGlobalTargetNode(GlobalCallTarget globalCallTarget) {
    this.globalCallTarget = globalCallTarget;
  }

  /**
   * Reads from the global call target.
   *
   * @param frame the stack frame for execution
   * @return the result of executing the call target
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (directCallNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();

      directCallNode = Truffle.getRuntime().createDirectCallNode(globalCallTarget.getTarget());
    }

    return directCallNode.call();
  }
}

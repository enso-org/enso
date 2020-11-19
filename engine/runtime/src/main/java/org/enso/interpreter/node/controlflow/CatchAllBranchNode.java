package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;

/**
 * This node represents an explicit catch-call case in a pattern match, as provided by the user. It
 * executes the catch-all case code.
 */
@NodeInfo(
    shortName = "Catch_All",
    description = "An explicit catch-all branch in a case expression")
public class CatchAllBranchNode extends BranchNode {

  private CatchAllBranchNode(RootCallTarget functionNode) {
    super(functionNode);
  }

  /**
   * Creates a node to handle the case catch-all.
   *
   * @param functionNode the function to execute in this case
   * @return a catch-all node
   */
  public static CatchAllBranchNode build(RootCallTarget functionNode) {
    return new CatchAllBranchNode(functionNode);
  }

  /**
   * Executes the case branch on an arbitrary target.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the object to match against
   */
  public void execute(VirtualFrame frame, Object state, Object target) {
    accept(frame, state, new Object[] {target});
  }
}

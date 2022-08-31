package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;

public class ObjectEqualityBranchNode extends BranchNode {
  private final Object expected;

  public static BranchNode build(RootCallTarget branch, Object expected) {
    return new ObjectEqualityBranchNode(branch, expected);
  }

  private ObjectEqualityBranchNode(RootCallTarget branch, Object expected) {
    super(branch);
    this.expected = expected;
  }

  @Override
  public void execute(VirtualFrame frame, Object state, Object target) {
    if (target == expected) {
      accept(frame, state, new Object[0]);
    }
  }
}

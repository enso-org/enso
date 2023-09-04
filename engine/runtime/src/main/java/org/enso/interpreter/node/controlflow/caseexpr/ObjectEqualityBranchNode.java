package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.expression.builtin.meta.IsSameObjectNode;

public class ObjectEqualityBranchNode extends BranchNode {
  private final Object expected;
  private @Child IsSameObjectNode isSameObject = IsSameObjectNode.build();
  private final CountingConditionProfile profile = CountingConditionProfile.create();

  private ObjectEqualityBranchNode(RootCallTarget branch, Object expected, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.expected = expected;
  }

  public static BranchNode build(RootCallTarget branch, Object expected, boolean terminalBranch) {
    return new ObjectEqualityBranchNode(branch, expected, terminalBranch);
  }

  @Override
  public void execute(VirtualFrame frame, Object state, Object target) {
    if (profile.profile(isSameObject.execute(target, expected))) {
      accept(frame, state, new Object[0]);
    }
  }
}

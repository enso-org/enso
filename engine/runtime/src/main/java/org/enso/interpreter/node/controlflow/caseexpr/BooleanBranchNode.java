package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;

/** An implementation of the case expression specialised to working on booleans. */
@NodeInfo(shortName = "BooleanMatch")
public abstract class BooleanBranchNode extends BranchNode {
  private final boolean matched;
  private final CountingConditionProfile profile = CountingConditionProfile.create();

  BooleanBranchNode(boolean matched, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.matched = matched;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matched the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static BooleanBranchNode build(
      boolean matched, RootCallTarget branch, boolean terminalBranch) {
    return BooleanBranchNodeGen.create(matched, branch, terminalBranch);
  }

  @Specialization
  void doAtom(VirtualFrame frame, Object state, boolean target) {
    if (profile.profile(matched == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}

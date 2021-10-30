package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;

/** An implementation of the case expression specialised to working on booleans. */
@NodeInfo(shortName = "BooleanMatch")
public abstract class BooleanBranchNode extends BranchNode {
  private final boolean matched;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  BooleanBranchNode(boolean matched, RootCallTarget branch) {
    super(branch);
    this.matched = matched;
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matched the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static BooleanBranchNode build(boolean matched, RootCallTarget branch) {
    return BooleanBranchNodeGen.create(matched, branch);
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

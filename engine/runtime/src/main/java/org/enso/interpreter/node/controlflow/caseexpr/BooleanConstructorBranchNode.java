package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "BooleanConsMatch", description = "Match using the Boolean constructor.")
public abstract class BooleanConstructorBranchNode extends BranchNode {
  private final Type boolTp;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  BooleanConstructorBranchNode(Type bool, RootCallTarget branch) {
    super(branch);
    this.boolTp = bool;
  }

  /**
   * Creates a new node for handling matching using the Boolean constructor in a case expression.
   *
   * @param bool the boolean container
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static BooleanConstructorBranchNode build(Type bool, RootCallTarget branch) {
    return BooleanConstructorBranchNodeGen.create(bool, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(target == boolTp)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization
  void doBool(VirtualFrame frame, Object state, boolean target) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}

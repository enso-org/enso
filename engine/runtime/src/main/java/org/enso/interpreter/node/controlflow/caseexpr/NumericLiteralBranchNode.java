package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigInteger;

@NodeInfo(shortName = "NumericLiteralMatch", description = "Allows matching on numeric literals")
public abstract class NumericLiteralBranchNode extends BranchNode {
  private final Object literal;

  private final ConditionProfile numProfile = ConditionProfile.createCountingProfile();

  NumericLiteralBranchNode(Object literal, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.literal = literal;
  }

  public static NumericLiteralBranchNode build(
      long literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  public static NumericLiteralBranchNode build(
      double literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  public static NumericLiteralBranchNode build(
      BigInteger literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  @Specialization(guards = "interop.isNumber(target)")
  void doObject(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "1") InteropLibrary interop) {
    if (numProfile.profile(literal == target)) accept(frame, state, new Object[0]);
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}
}

package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.data.text.Text;

import com.ibm.icu.text.Normalizer;

@NodeInfo(shortName = "StringLiteralMatch", description = "Allows matching on String literals")
public abstract class StringLiteralBranchNode extends BranchNode {
  private final String literal;

  private final ConditionProfile textProfile = ConditionProfile.createCountingProfile();

  StringLiteralBranchNode(String literal, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.literal = literal;
  }

  public static StringLiteralBranchNode build(
      String literal, RootCallTarget branch, boolean terminalBranch) {
    return StringLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  @Specialization
  void doText(
      VirtualFrame frame,
      Object state,
      Text target,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    if (textProfile.profile(equalStrings(literal, toJavaStringNode.execute(target)))) {
      accept(frame, state, new Object[0]);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private boolean equalStrings(String s1, String s2) {
    return Normalizer.compare(s1, s2, Normalizer.FOLD_CASE_DEFAULT) == 0;
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}
}

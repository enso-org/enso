package org.enso.interpreter.node.controlflow.caseexpr;

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

  StringLiteralBranchNode(String literal, RootCallTarget branch) {
    super(branch);
    this.literal = literal;
  }

  public static StringLiteralBranchNode build(String literal, RootCallTarget branch) {
    return StringLiteralBranchNodeGen.create(literal, branch);
  }

  @Specialization
  void doText(
      VirtualFrame frame,
      Object state,
      Text target,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    if (textProfile.profile(
        Normalizer.compare(literal, toJavaStringNode.execute(target), Normalizer.FOLD_CASE_DEFAULT)
            == 0)) accept(frame, state, new Object[0]);
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}
}

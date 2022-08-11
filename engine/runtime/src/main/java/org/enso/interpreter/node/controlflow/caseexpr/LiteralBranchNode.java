package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.data.text.Text;

import java.math.BigInteger;

@NodeInfo(shortName = "LiteralMatch", description = "Allows matching on literals")
public abstract class LiteralBranchNode extends BranchNode {
  private final Object literal;

  private final ConditionProfile textProfile = ConditionProfile.createCountingProfile();
  private final ConditionProfile numProfile = ConditionProfile.createCountingProfile();

  LiteralBranchNode(Object literal, RootCallTarget branch) {
    super(branch);
    this.literal = literal;
  }

  public static LiteralBranchNode build(long literal, RootCallTarget branch) {
    return LiteralBranchNodeGen.create(literal, branch);
  }

  public static LiteralBranchNode build(double literal, RootCallTarget branch) {
    return LiteralBranchNodeGen.create(literal, branch);
  }

  public static LiteralBranchNode build(BigInteger literal, RootCallTarget branch) {
    return LiteralBranchNodeGen.create(literal, branch);
  }

  public static LiteralBranchNode build(String literal, RootCallTarget branch) {
    return LiteralBranchNodeGen.create(literal, branch);
  }

  @Specialization
  void doText(
      VirtualFrame frame,
      Object state,
      Text target,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    if (textProfile.profile(literal.equals(toJavaStringNode.execute(target))))
      accept(frame, state, new Object[0]);
  }

  @Specialization(guards = "numbers.isNumber(target)")
  void doObject(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "1") InteropLibrary numbers) {
    if (numProfile.profile(literal == target)) accept(frame, state, new Object[0]);
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}
}

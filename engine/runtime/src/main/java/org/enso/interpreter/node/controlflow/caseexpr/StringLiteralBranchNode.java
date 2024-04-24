package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.polyglot.common_utils.Core_Text_Utils;

@NodeInfo(shortName = "StringLiteralMatch", description = "Allows matching on String literals")
public abstract class StringLiteralBranchNode extends BranchNode {
  private final String literal;

  private final CountingConditionProfile textProfile = CountingConditionProfile.create();

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

  @Specialization(
      guards = {"targetInterop.isString(target)"},
      limit = "3")
  void doInteropString(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary("target") InteropLibrary targetInterop) {
    try {
      if (textProfile.profile(equalStrings(literal, targetInterop.asString(target)))) {
        accept(frame, state, new Object[0]);
      }
    } catch (UnsupportedMessageException e) {
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(this, null, e);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private boolean equalStrings(String s1, String s2) {
    return Core_Text_Utils.equals(s1, s2);
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}
}

package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.data.Type;

@NodeInfo(shortName = "TextMatch", description = "Allows matching on the Text type.")
public abstract class TextBranchNode extends BranchNode {
  private final Type text;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  TextBranchNode(Type text, RootCallTarget branch) {
    super(branch);
    this.text = text;
  }

  /**
   * Creates a new node for handling matching on a text in a case expression.
   *
   * @param text the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching on text in a case expression
   */
  public static TextBranchNode build(Type text, RootCallTarget branch) {
    return TextBranchNodeGen.create(text, branch);
  }

  @Specialization
  void doConstructor(VirtualFrame frame, Object state, Type target) {
    if (profile.profile(text == target)) {
      accept(frame, state, new Object[0]);
    }
  }

  @Specialization(guards = "strings.isString(target)")
  void doLiteral(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "10") InteropLibrary strings) {
    accept(frame, state, new Object[0]);
  }

  @Fallback
  void doFallback(VirtualFrame frame, Object state, Object target) {}
}

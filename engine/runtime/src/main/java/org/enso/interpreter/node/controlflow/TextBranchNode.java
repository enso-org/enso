package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "TextMatch", description = "Allows matching on the Text type.")
public abstract class TextBranchNode extends BranchNode {
  private final AtomConstructor text;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  TextBranchNode(AtomConstructor text, RootCallTarget branch) {
    this.text = text;
    callNode = DirectCallNode.create(branch);
  }

  /**
   * Creates a new node for handling matching on a text in a case expression.
   *
   * @param text the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching on text in a case expression
   */
  public static TextBranchNode build(AtomConstructor text, RootCallTarget branch) {
    return TextBranchNodeGen.create(text, branch);
  }

  /**
   * Handles the atom scrutinee case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doConstructor(VirtualFrame frame, Object state, Atom target) {
    if (profile.profile(text == target.getConstructor())) {
      // Note [Caller Info For Case Branches]
      Stateful result =
          (Stateful)
              callNode.call(
                  Function.ArgumentsHelper.buildArguments(
                      frame.materialize(), state, target.getFields()));
      throw new BranchSelectedException(result);
    }
  }

  /**
   * Handles the text instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doLiteral(VirtualFrame frame, Object state, Text target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[] {}));
    throw new BranchSelectedException(result);
  }

  /**
   * The fallback specialisation for executing the text branch node.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}
}

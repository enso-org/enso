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
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "TextMatch", description = "Allows matching on the Decimal type.")
public abstract class DecimalBranchNode extends BranchNode {
  private final AtomConstructor decimal;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  DecimalBranchNode(AtomConstructor decimal, RootCallTarget branch) {
    this.decimal = decimal;
    this.callNode = DirectCallNode.create(branch);
  }

  /**
   * Create a new node to handle matching with the Decimal constructor.
   *
   * @param decimal the constructor used for matching
   * @param branch the code to execute in this case
   * @return a decimal branch node
   */
  public static DecimalBranchNode build(AtomConstructor decimal, RootCallTarget branch) {
    return DecimalBranchNodeGen.create(decimal, branch);
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
    if (profile.profile(decimal == target.getConstructor())) {
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
   * Handles the decimal instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doLiteral(VirtualFrame frame, Object state, double target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[] {}));
    throw new BranchSelectedException(result);
  }

  /**
   * The fallback specialisation for executing the decimal branch node.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}
}

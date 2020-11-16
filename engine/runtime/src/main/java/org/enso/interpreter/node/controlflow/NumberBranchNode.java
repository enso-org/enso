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
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "NumberMatch", description = "Allows matching on the Number type.")
public abstract class NumberBranchNode extends BranchNode {
  private final AtomConstructor number;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  NumberBranchNode(AtomConstructor number, RootCallTarget branch) {
    this.number = number;
    this.callNode = DirectCallNode.create(branch);
  }

  /**
   * Create a new node to handle matching with the Number constructor.
   *
   * @param number the constructor used for matching
   * @param branch the code to execute
   * @return an integer branch node
   */
  public static NumberBranchNode build(AtomConstructor number, RootCallTarget branch) {
    return NumberBranchNodeGen.create(number,branch);
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
    if (profile.profile(number == target.getConstructor())) {
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
   * Handles the small integer instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doSmallInteger(VirtualFrame frame, Object state, long target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[]{}));
    throw new BranchSelectedException(result);
  }

  /**
   * Handles the big integer instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doBigInteger(VirtualFrame frame, Object state, EnsoBigInteger target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[]{}));
    throw new BranchSelectedException(result);
  }

  /**
   * Handles the decimal instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doDecimal(VirtualFrame frame, Object state, double target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[]{}));
    throw new BranchSelectedException(result);
  }

  /**
   * The fallback specialisation for executing the number branch node.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}
}

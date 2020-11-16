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
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.state.Stateful;

@NodeInfo(shortName = "ArrayMatch", description = "Allows matching on the Array type.")
public abstract class ArrayBranchNode extends BranchNode {
  private final AtomConstructor array;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  ArrayBranchNode(AtomConstructor array, RootCallTarget branch) {
    this.array = array;
    this.callNode = DirectCallNode.create(branch);
  }

  /**
   * Create a new node to handle matching with the Array constructor.
   *
   * @param array the constructor used for matching in this case
   * @param branch the code to execute in this case
   * @return an array branch node
   */
  public static ArrayBranchNode build(AtomConstructor array, RootCallTarget branch) {
    return ArrayBranchNodeGen.create(array, branch);
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
    if (profile.profile(array == target.getConstructor())) {
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
   * Handles the array instance case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doArray(VirtualFrame frame, Object state, Array target) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(
                    frame.materialize(), state, new Object[] {}));
    throw new BranchSelectedException(result);
  }

  /**
   * The fallback specialisation for executing the array branch node.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}
}

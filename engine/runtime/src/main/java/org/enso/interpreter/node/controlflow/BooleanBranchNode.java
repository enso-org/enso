package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.type.TypesGen;

/** An implementation of the case expression specialised to working on booleans. */
@NodeInfo(shortName = "BooleanMatch")
public abstract class BooleanBranchNode extends BranchNode {
  private final boolean matched;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();
  private @Child DirectCallNode callNode;

  BooleanBranchNode(boolean matched, RootCallTarget branch) {
    this.matched = matched;
    this.callNode = DirectCallNode.create(branch);
  }

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matched the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   * @return a node for matching in a case expression
   */
  public static BooleanBranchNode build(boolean matched, RootCallTarget branch) {
    return BooleanBranchNodeGen.create(matched, branch);
  }

  /**
   * Handles the boolean scrutinee case.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the atom to destructure
   */
  @Specialization
  public void doAtom(VirtualFrame frame, Object state, boolean target) {
    if (profile.profile(matched == target)) {
      Stateful result =
          (Stateful)
              callNode.call(
                  Function.ArgumentsHelper.buildArguments(
                      frame.materialize(), state, new Object[0]));
      // Note [Caller Info For Case Branches]
      throw new BranchSelectedException(result);
    }
  }

  /**
   * The fallback specialisation for executing the boolean branch node.
   *
   * @param frame the stack frame in which to execute
   * @param target the object to execute on
   */
  @Fallback
  public void doFallback(VirtualFrame frame, Object state, Object target) {}

  /* Note [Caller Info For Case Branches]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * It is assumed that functions serving as pattern match logic branches are always function
   * literals, not references, curried functions etc. Therefore, as function literals, they
   * have no way of accessing the caller frame and can safely be passed null.
   */
}

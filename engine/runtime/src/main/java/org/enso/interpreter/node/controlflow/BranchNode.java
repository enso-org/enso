package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.Stateful;

/** An abstract representation of a case branch. */
@NodeInfo(shortName = "case_branch", description = "Represents a case branch at runtime.")
public abstract class BranchNode extends BaseNode {
  private @Child DirectCallNode callNode;

  BranchNode(RootCallTarget branch) {
    this.callNode = DirectCallNode.create(branch);
  }

  /**
   * Executes the case branch.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param target the object to match against
   */
  public abstract void execute(VirtualFrame frame, Object state, Object target);

  /**
   * Accepts the case branch, continuing the execution of the case expression.
   *
   * @param frame the stack frame in which to execute
   * @param state current monadic state
   * @param args the arguments to be passed to the branch body
   */
  protected void accept(VirtualFrame frame, Object state, Object[] args) {
    // Note [Caller Info For Case Branches]
    Stateful result =
        (Stateful)
            callNode.call(
                Function.ArgumentsHelper.buildArguments(frame.materialize(), state, args));
    throw new BranchSelectedException(result);
  }

  /* Note [Caller Info For Case Branches]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * It is assumed that functions serving as pattern match logic branches are always function
   * literals, not references, curried functions etc. Therefore, as function literals, they
   * have no way of accessing the caller frame and can safely be passed null.
   */

  /* Note [Safe Casting to Function in Catch All Branches]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * The syntactic nature of a catch all node guarantees that it has _only one_
   * matcher in its pattern, regardless of whether it is named or a blank. As
   * a result, we _know_ that the expression of the branch will _always_ be a
   * function at code generation time, and hence we know that we can safely cast
   * it to a function during execution.
   */
}

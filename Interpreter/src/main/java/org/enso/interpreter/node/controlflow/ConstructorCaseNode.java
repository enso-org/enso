package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

/** An implementation of the case expression specialised to working on constructors. */
public class ConstructorCaseNode extends CaseNode {
  @Child private ExpressionNode matcher;
  @Child private ExpressionNode branch;
  @Child private ExecuteCallNode executeCallNode = ExecuteCallNodeGen.create();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  /**
   * Creates a new node for handling matching on a case expression.
   *
   * @param matcher the expression to use for matching
   * @param branch the expression to be executed if (@code matcher} matches
   */
  public ConstructorCaseNode(ExpressionNode matcher, ExpressionNode branch) {
    this.matcher = matcher;
    this.branch = branch;
  }

  /**
   * Sets whether or not the case expression is tail recursive.
   *
   * @param isTail whether or not the case expression is tail-recursive
   */
  @Override
  public void setTail(boolean isTail) {
    branch.setTail(isTail);
  }

  /**
   * Handles the atom scrutinee case.
   *
   * <p>The atom's constructor is checked and if it matches the conditional branch is executed with
   * all the atom's fields as arguments.
   *
   * @param frame the stack frame in which to execute
   * @param target the atom to destructure
   * @throws UnexpectedResultException
   */
  @Override
  public void executeAtom(VirtualFrame frame, Atom target) throws UnexpectedResultException {
    AtomConstructor matcherVal = matcher.executeAtomConstructor(frame);
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    if (profile.profile(matcherVal == target.getConstructor())) {
      Function function = branch.executeFunction(frame);
      throw new BranchSelectedException(
          executeCallNode.executeCall(function, state, target.getFields()));
    }
  }

  /**
   * Handles the function scrutinee case, by not matching it at all.
   *
   * @param frame the stack frame in which to execute
   * @param target the function to match
   */
  @Override
  public void executeFunction(VirtualFrame frame, Function target) {}

  /**
   * Handles the number scrutinee case, by not matching it at all.
   *
   * @param frame the stack frame in which to execute
   * @param target the function to match
   */
  @Override
  public void executeNumber(VirtualFrame frame, long target) {}
}

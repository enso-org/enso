package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.callable.ExecuteCallNodeGen;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

/** An implementation of the case expression specialised to working on explicit constructors. */
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
   * Executes the case expression.
   *
   * <p>It has no direct return value and instead uses a {@link BranchSelectedException} to signal
   * the correct result back to the parent of the case expression.
   *
   * @param frame the stack frame in which to execute
   * @param target the constructor to destructure
   * @throws UnexpectedResultException when the result of desctructuring {@code target} can't be
   *     represented as a value of the expected return type
   */
  public void execute(VirtualFrame frame, Atom target) throws UnexpectedResultException {
    AtomConstructor matcherVal = matcher.executeAtomConstructor(frame);
    if (profile.profile(matcherVal == target.getConstructor())) {
      Function function = branch.executeFunction(frame);
      throw new BranchSelectedException(executeCallNode.executeCall(function, target.getFields()));
    }
  }
}

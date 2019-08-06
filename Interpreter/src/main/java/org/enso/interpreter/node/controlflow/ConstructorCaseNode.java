package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.function.CallNode;
import org.enso.interpreter.node.function.CallNodeGen;
import org.enso.interpreter.runtime.Atom;
import org.enso.interpreter.runtime.AtomConstructor;
import org.enso.interpreter.runtime.Function;

public class ConstructorCaseNode extends CaseNode {
  @Child private ExpressionNode matcher;
  @Child private ExpressionNode branch;
  @Child private CallNode callNode = CallNodeGen.create();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  public ConstructorCaseNode(ExpressionNode matcher, ExpressionNode branch) {
    this.matcher = matcher;
    this.branch = branch;
  }

  @Override
  public void markTail() {
    branch.markTail();
  }

  public void execute(VirtualFrame frame, Atom target) throws UnexpectedResultException {
    AtomConstructor matcherVal = matcher.executeAtomConstructor(frame);
    if (profile.profile(matcherVal == target.getConstructor())) {
      Function function = branch.executeFunction(frame);
      throw new BranchSelectedException(callNode.executeCall(function, target.getFields()));
    }
  }
}

package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.runtime.TypeError;
import org.enso.interpreter.node.ExpressionNode;

@NodeInfo(shortName = "if_then_else", description = "if arg0 = 0 then arg1 else arg2")
public class IfZeroNode extends ExpressionNode {
  @Child private ExpressionNode condition;
  @Child private ExpressionNode ifTrue;
  @Child private ExpressionNode ifFalse;

  private final ConditionProfile conditionProf = ConditionProfile.createCountingProfile();

  public IfZeroNode(ExpressionNode condition, ExpressionNode ifTrue, ExpressionNode ifFalse) {
    this.condition = condition;
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }

  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (conditionProf.profile(executeCondition(frame) == 0)) {
      return ifTrue.executeGeneric(frame);
    } else {
      return ifFalse.executeGeneric(frame);
    }
  }

  private long executeCondition(VirtualFrame frame) {
    try {
      return condition.executeLong(frame);
    } catch (UnexpectedResultException ex) {
      throw new TypeError("Type error, expected long", this);
    }
  }

  @Override
  public void markTail() {
    ifTrue.markTail();
    ifFalse.markTail();
  }
}

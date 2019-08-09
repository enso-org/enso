package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.error.TypeError;

/**
 * This node represents a very simple form of the if-then-else expression, specialised to the case
 * of checking whether its input is equal to zero.
 */
@NodeInfo(shortName = "if_then_else", description = "if arg0 = 0 then arg1 else arg2")
public class IfZeroNode extends ExpressionNode {
  private final ConditionProfile conditionProf = ConditionProfile.createCountingProfile();
  @Child private ExpressionNode condition;
  @Child private ExpressionNode ifTrue;
  @Child private ExpressionNode ifFalse;

  /**
   * Creates a new conditional expression node.
   *
   * @param condition the condition to execute and check for equality with zero
   * @param ifTrue the code to execute if {@code condition} is true
   * @param ifFalse the code to execute if {@code condition} is false
   */
  public IfZeroNode(ExpressionNode condition, ExpressionNode ifTrue, ExpressionNode ifFalse) {
    this.condition = condition;
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }

  /**
   * Executes the conditional expression.
   *
   * @param frame the stack frame for execution
   * @return the result of the {@code isTrue} branch if the condition is {@code true}, otherwise the
   *     result of the {@code isFalse} branch
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    if (conditionProf.profile(executeCondition(frame) == 0)) {
      return ifTrue.executeGeneric(frame);
    } else {
      return ifFalse.executeGeneric(frame);
    }
  }

  /**
   * Executes the condition to be checked.
   *
   * @param frame the stack frame for execution
   * @return the result of executing the condition
   */
  private long executeCondition(VirtualFrame frame) {
    try {
      return condition.executeLong(frame);
    } catch (UnexpectedResultException ex) {
      throw new TypeError("Type error, expected long", this);
    }
  }

  /**
   * Sets whether the conditional is tail-recursive or not.
   *
   * @param isTail whether or not the conditional is tail recursive
   */
  @Override
  public void setTail(boolean isTail) {
    ifTrue.setTail(isTail);
    ifFalse.setTail(isTail);
  }
}

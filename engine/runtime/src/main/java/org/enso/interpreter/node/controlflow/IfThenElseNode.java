package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.thunk.ForceNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.Stateful;

@NodeChild(value = "condition", type = ExpressionNode.class)
@NodeInfo(shortName = "if_then_else", description = "An optimised if-then-else on Boolean.")
public abstract class IfThenElseNode extends ExpressionNode {
  private @Child ExpressionNode onTrue;
  private @Child ThunkExecutorNode thunkOnTrue;
  private @Child ExpressionNode onFalse;
  private @Child ThunkExecutorNode thunkOnFalse;
  private @Child InvokeCallableNode onFunction;
  private final UnresolvedSymbol ifThenElse;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  IfThenElseNode(ExpressionNode onTrue, ExpressionNode onFalse, UnresolvedSymbol ifThenElse) {
    this.onTrue = onTrue;
    this.thunkOnTrue = ThunkExecutorNode.build();
    this.onFalse = onFalse;
    this.thunkOnFalse = ThunkExecutorNode.build();
    this.onFunction =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {
              new CallArgumentInfo(), new CallArgumentInfo(), new CallArgumentInfo(),
            },
            DefaultsExecutionMode.EXECUTE,
            ArgumentsExecutionMode.EXECUTE);
    this.ifThenElse = ifThenElse;
  }

  /**
   * Create a new optimised representation of the if-then-else expression on booleans.
   *
   * @param condition the condition of the expression
   * @param onTrue the branch to execute when {@code condition} is true
   * @param onFalse the branch to execute when {@code condition} is false
   * @param ifThenElse An unresolved symbol representing {@code if_then_else} in the scope where it
   *     is being called
   * @return a new {@code IfThenElseNode}
   */
  public static IfThenElseNode build(
      ForceNode condition,
      ExpressionNode onTrue,
      ExpressionNode onFalse,
      UnresolvedSymbol ifThenElse) {
    return IfThenElseNodeGen.create(onTrue, onFalse, ifThenElse, condition);
  }

  @Specialization
  Object doBoolean(VirtualFrame frame, boolean condition) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    if (profile.profile(condition)) {
      Stateful result =
          thunkOnTrue.executeThunk(onTrue.executeGeneric(frame), state, getTailStatus());
      frame.setObject(getStateFrameSlot(), result.getState());
      return result.getValue();
    } else {
      Stateful result =
          thunkOnFalse.executeThunk(onFalse.executeGeneric(frame), state, getTailStatus());
      frame.setObject(getStateFrameSlot(), result.getState());
      return result.getValue();
    }
  }

  @Fallback
  Object doOther(VirtualFrame frame, Object condition) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    var executedOnTrue = onTrue.executeGeneric(frame);
    var executedOnFalse = onFalse.executeGeneric(frame);
    Stateful result =
        onFunction.execute(
            ifThenElse, frame, state, new Object[] {condition, executedOnTrue, executedOnFalse});
    frame.setObject(getStateFrameSlot(), result.getState());
    return result.getValue();
  }
}

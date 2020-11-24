package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.thunk.ForceNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.Stateful;

@NodeChild(value = "condition", type = ExpressionNode.class)
@NodeInfo(shortName = "if_then_else", description = "An optimised if-then-else on Boolean.")
public abstract class IfThenNode extends ExpressionNode {
  private @Child ExpressionNode onTrue;
  private @Child ThunkExecutorNode thunkOnTrue;
  private @Child InvokeCallableNode onFunction;
  private final UnresolvedSymbol ifThen;
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  IfThenNode(ExpressionNode onTrue, UnresolvedSymbol ifThen) {
    this.onTrue = onTrue;
    this.thunkOnTrue = ThunkExecutorNode.build();
    this.onFunction =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo(), new CallArgumentInfo()},
            DefaultsExecutionMode.EXECUTE,
            ArgumentsExecutionMode.EXECUTE);
    this.ifThen = ifThen;
  }

  /**
   * Create a new optimised representation of the if-then-else expression on booleans.
   *
   * @param condition the condition of the expression
   * @param onTrue the branch to execute when {@code condition} is true
   * @param ifThenElse An unresolved symbol representing {@code if_then_else} in the scope where it
   *     is being called
   * @return a new {@code IfThenElseNode}
   */
  public static IfThenNode build(
      ForceNode condition, ExpressionNode onTrue, UnresolvedSymbol ifThenElse) {
    return IfThenNodeGen.create(onTrue, ifThenElse, condition);
  }

  @Specialization
  Object doBoolean(
      VirtualFrame frame,
      boolean condition,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    if (profile.profile(condition)) {
      Stateful result =
          thunkOnTrue.executeThunk(onTrue.executeGeneric(frame), state, getTailStatus());
      frame.setObject(getStateFrameSlot(), result.getState());
      return result.getValue();
    } else {
      return ctxRef.get().getBuiltins().nothing().newInstance();
    }
  }

  @Fallback
  Object doOther(VirtualFrame frame, Object condition) {
    Object state = FrameUtil.getObjectSafe(frame, getStateFrameSlot());
    var executedOnTrue = onTrue.executeGeneric(frame);
    Stateful result =
        onFunction.execute(ifThen, frame, state, new Object[] {condition, executedOnTrue});
    frame.setObject(getStateFrameSlot(), result.getState());
    return result.getValue();
  }
}

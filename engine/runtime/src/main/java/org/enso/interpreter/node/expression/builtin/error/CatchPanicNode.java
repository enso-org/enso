package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Panic",
    name = "catch",
    description = "Executes an action if a panic was thrown, calls the provided callback.")
public abstract class CatchPanicNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();
  private @Child IsPayloadOfPanicTypeNode isPayloadOfPanicTypeNode =
      IsPayloadOfPanicTypeNode.build();
  private @Child ThrowPanicNode throwPanicNode = ThrowPanicNode.build();
  private final ConditionProfile profile = ConditionProfile.createCountingProfile();

  CatchPanicNode() {
    this.invokeCallableNode =
        InvokeCallableNode.build(
            new CallArgumentInfo[] {new CallArgumentInfo()},
            InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
            InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
    this.invokeCallableNode.setTailStatus(BaseNode.TailStatus.TAIL_DIRECT);
  }

  static CatchPanicNode build() {
    return CatchPanicNodeGen.create();
  }

  abstract Object execute(
      VirtualFrame frame, State state, Object panicType, @Suspend Object action, Object handler);

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      State state,
      Object panicType,
      Object action,
      Object handler,
      @Cached BranchProfile panicBranchProfile,
      @Cached BranchProfile otherExceptionBranchProfile) {
    try {
      return thunkExecutorNode.executeThunk(action, state, BaseNode.TailStatus.TAIL_DIRECT);
    } catch (PanicException e) {
      panicBranchProfile.enter();
      Object payload = e.getPayload();
      return executeCallback(frame, state, panicType, handler, payload, e);
    } catch (AbstractTruffleException e) {
      otherExceptionBranchProfile.enter();
      return executeCallback(frame, state, panicType, handler, e, e);
    }
  }

  private Object executeCallback(
      VirtualFrame frame,
      State state,
      Object panicType,
      Object handler,
      Object payload,
      AbstractTruffleException originalException) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    Atom caughtPanic =
        builtins.caughtPanic().getUniqueConstructor().newInstance(payload, originalException);

    if (profile.profile(isPayloadOfPanicTypeNode.execute(panicType, payload))) {
      return invokeCallableNode.execute(handler, frame, state, new Object[] {caughtPanic});
    } else {
      return throwPanicNode.execute(caughtPanic);
    }
  }
}

package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
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
    name = "catch_primitive",
    description = "Executes an action if a panic was thrown, calls the provided callback.")
public abstract class CatchPanicNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

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

  abstract Object execute(VirtualFrame frame, State state, @Suspend Object action, Object handler);

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      State state,
      Object action,
      Object handler,
      @Cached BranchProfile panicBranchProfile,
      @Cached BranchProfile otherExceptionBranchProfile) {
    try {
      return thunkExecutorNode.executeThunk(action, state, BaseNode.TailStatus.TAIL_DIRECT);
    } catch (PanicException e) {
      panicBranchProfile.enter();
      Object payload = e.getPayload();
      return executeCallback(frame, state, handler, payload, e);
    } catch (AbstractTruffleException e) {
      otherExceptionBranchProfile.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      Object payload = builtins.error().getPolyglotError().wrap(e);
      return executeCallback(frame, state, handler, payload, e);
    }
  }

  private Object executeCallback(
      VirtualFrame frame,
      State state,
      Object handler,
      Object payload,
      AbstractTruffleException originalException) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    Atom caughtPanic =
        builtins.caughtPanic().getUniqueConstructor().newInstance(payload, originalException);
    return invokeCallableNode.execute(handler, frame, state, new Object[] {caughtPanic});
  }
}

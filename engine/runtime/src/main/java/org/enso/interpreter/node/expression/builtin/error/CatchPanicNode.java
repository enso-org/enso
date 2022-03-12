package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Panic",
    name = "catch",
    description = "Executes an action if a panic was thrown, calls the provided callback.")
public abstract class CatchPanicNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode;
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();
  private final BranchProfile unknownExceptionProfile = BranchProfile.create();

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

  abstract Stateful execute(
      VirtualFrame frame, @MonadicState Object state, Object _this, @Suspend Object action, Object handler);

  @Specialization
  Stateful doExecute(
      VirtualFrame frame,
      @MonadicState Object state,
      Object _this,
      Object action,
      Object handler,
      @CachedLibrary(limit = "5") InteropLibrary exceptions) {
    try {
      return thunkExecutorNode.executeThunk(action, state, BaseNode.TailStatus.NOT_TAIL);
    } catch (PanicException e) {
      return invokeCallableNode.execute(handler, frame, state, new Object[] {e.getPayload()});
    } catch (Throwable e) {
      if (exceptions.isException(e)) {
        Object payload = Context.get(this).getBuiltins().error().makePolyglotError(e);
        return invokeCallableNode.execute(handler, frame, state, new Object[] {payload});
      }
      unknownExceptionProfile.enter();
      throw e;
    }
  }
}

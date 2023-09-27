package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Runtime",
    name = "assert_builtin",
    description = "Asserts that the given condition is true",
    autoRegister = false)
public abstract class AssertNode extends Node {

  public static AssertNode build() {
    return AssertNodeGen.create();
  }

  public abstract Object execute(
      VirtualFrame frame, State state, @Suspend Object action, Text msg);

  protected boolean isAssertionsEnabled() {
    return EnsoContext.get(this).isAssertionsEnabled();
  }

  @Specialization(guards = "!isAssertionsEnabled()")
  Object doAssertionsDisabled(VirtualFrame frame, State state, Object action, Text msg) {
    return EnsoContext.get(this).getNothing();
  }

  @Specialization(replaces = "doAssertionsDisabled")
  Object doAssertionsEnabled(
      VirtualFrame frame,
      State state,
      Object action,
      Text msg,
      @Cached("create()") ThunkExecutorNode thunkExecutorNode,
      @CachedLibrary(limit = "3") InteropLibrary interop,
      @Cached GetStackTraceNode getStackTraceNode) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    Object actionRes =
        thunkExecutorNode.executeThunk(frame, action, state, BaseNode.TailStatus.TAIL_DIRECT);
    try {
      if (interop.asBoolean(actionRes)) {
        return ctx.getNothing();
      } else {
        var stackTrace = getStackTraceNode.execute(frame);
        throw new PanicException(
            builtins.error().makeAssertionError(msg, stackTrace),
            this
        );
      }
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      return builtins
          .error()
          .makeTypeError(
              builtins.bool(),
              TypeOfNode.getUncached().execute(actionRes),
              "Result of `action` parameter");
    }
  }
}

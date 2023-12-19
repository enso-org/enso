package org.enso.interpreter.node.expression.builtin.runtime;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
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

  public abstract Object execute(VirtualFrame frame, State state, @Suspend Object action, Text msg);

  @Idempotent
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
      @Cached BranchProfile resultIsNotAtomProfile) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    Object actionRes =
        thunkExecutorNode.executeThunk(frame, action, state, BaseNode.TailStatus.TAIL_DIRECT);
    if (actionRes instanceof Atom resAtom) {
      var isTrue = resAtom.getConstructor() == builtins.bool().getTrue();
      if (isTrue) {
        return ctx.getNothing();
      } else {
        throw new PanicException(builtins.error().makeAssertionError(msg), this);
      }
    } else {
      resultIsNotAtomProfile.enter();
      return checkResultSlowPath(actionRes, msg);
    }
  }

  @TruffleBoundary
  private Object checkResultSlowPath(Object actionRes, Text msg) {
    var ctx = EnsoContext.get(this);
    var builtins = ctx.getBuiltins();
    try {
      if (InteropLibrary.getUncached().asBoolean(actionRes)) {
        return ctx.getNothing();
      } else {
        throw new PanicException(builtins.error().makeAssertionError(msg), this);
      }
    } catch (UnsupportedMessageException e) {
      if (actionRes instanceof DataflowError dataflowError) {
        var txt = Text.create("Result of assert action is a dataflow error: " + dataflowError);
        throw new PanicException(builtins.error().makeAssertionError(txt), this);
      } else {
        var typeError =
            builtins
                .error()
                .makeTypeError(
                    builtins.bool().getType(),
                    TypeOfNode.getUncached().execute(actionRes),
                    "Result of assert action");
        throw new PanicException(typeError, this);
      }
    }
  }
}

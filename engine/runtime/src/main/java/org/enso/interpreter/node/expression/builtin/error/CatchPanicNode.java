package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Panic",
    name = "catch",
    description = "Executes an action and converts any Panic thrown by it into an Error")
public abstract class CatchPanicNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  static CatchPanicNode build() {
    return CatchPanicNodeGen.create();
  }

  abstract Stateful execute(@MonadicState Object state, Object _this, Thunk action);

  @Specialization
  Stateful doExecute(
      @MonadicState Object state,
      Object _this,
      Thunk action,
      @CachedContext(Language.class) Context ctx) {
    try {
      return thunkExecutorNode.executeThunk(action, state, false);
    } catch (PanicException e) {
      return new Stateful(state, new RuntimeError(e.getExceptionObject()));
    } catch (Throwable e) {
      if (ctx.getEnvironment().isHostException(e)) {
        Object cause = ((TruffleException) e).getExceptionObject();
        return new Stateful(
            state, new RuntimeError(ctx.getBuiltins().error().makePolyglotError(cause)));
      }
      throw e;
    }
  }
}

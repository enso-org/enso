package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Panic",
    name = "catch",
    description = "Executes an action and converts any Panic thrown by it into an Error")
public class CatchPanicNode extends Node {
  private @Child ThunkExecutorNode thunkExecutorNode = ThunkExecutorNode.build();

  Stateful execute(@MonadicState Object state, Object _this, Thunk action) {
    try {
      return thunkExecutorNode.executeThunk(action, state, false);
    } catch (PanicException e) {
      return new Stateful(state, new RuntimeError(e.getExceptionObject()));
    }
  }
}

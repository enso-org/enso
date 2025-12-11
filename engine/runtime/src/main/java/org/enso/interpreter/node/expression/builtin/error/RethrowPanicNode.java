package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "Panic", name = "rethrow", description = "Turns error into Panic.")
public abstract class RethrowPanicNode extends Node {

  RethrowPanicNode() {}

  static RethrowPanicNode build() {
    return RethrowPanicNodeGen.create();
  }

  abstract Object execute(VirtualFrame frame, State state, @AcceptsError Object obj);

  @Specialization
  Object doExecute(VirtualFrame frame, State state, Object obj) {
    if (obj instanceof DataflowError err) {
      throw err.rethrow();
    }
    return obj;
  }
}

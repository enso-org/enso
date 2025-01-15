package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.HasContextEnabledNode;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Error",
    name = "throw",
    description = "Returns a new value error with given payload.",
    inlineable = true)
public class ThrowErrorNode extends Node {
  private @Child HasContextEnabledNode hasContextEnabledNode = HasContextEnabledNode.create();

  public Object execute(VirtualFrame giveMeAStackFrame, State state, Object payload) {
    return DataflowError.withDefaultTrace(state, payload, this, hasContextEnabledNode);
  }
}

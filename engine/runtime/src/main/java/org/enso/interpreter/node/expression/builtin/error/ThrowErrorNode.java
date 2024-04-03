package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Error",
    name = "throw",
    description = "Returns a new value error with given payload.",
    inlineable = true)
public class ThrowErrorNode extends Node {
  public Object execute(VirtualFrame giveMeAStackFrame, State state, Object payload) {
    boolean lala = state.currentEnvironment().hasContextEnabled("Dataflow_Stack_Trace");
    EnsoContext context = EnsoContext.get(this);
    String skeys[] = state.currentEnvironment().keys;
    String ckeys[] = context.getExecutionEnvironment().keys;
    boolean attachFullStackTrace = context.getExecutionEnvironment().hasContextEnabled("Dataflow_Stack_Trace");
    return DataflowError.withoutTrace(payload, this);
  }
}

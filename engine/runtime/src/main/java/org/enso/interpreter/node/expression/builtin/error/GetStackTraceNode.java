package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Error",
    name = "primitive_get_stack_trace",
    description = "Gets the stack trace of the error's origin.")
public class GetStackTraceNode extends Node {
  Array execute(DataflowError _this) {
    return org.enso.interpreter.node.expression.builtin.runtime.GetStackTraceNode.stackTraceToArray(
        _this);
  }
}

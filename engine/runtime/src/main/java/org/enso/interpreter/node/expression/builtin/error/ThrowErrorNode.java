package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.RuntimeError;

@BuiltinMethod(
    type = "Error",
    name = "throw",
    description = "Returns a new value error with given payload.")
public class ThrowErrorNode extends Node {
  public Object execute(Object _this, Object payload) {
    return new RuntimeError(payload);
  }
}

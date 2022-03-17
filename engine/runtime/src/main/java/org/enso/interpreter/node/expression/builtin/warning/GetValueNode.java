package org.enso.interpreter.node.expression.builtin.warning;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.Warning;

@BuiltinMethod(
    type = "Prim_Warning",
    name = "get_value",
    description = "Gets the payload of the warning.")
public class GetValueNode extends Node {
  Object execute(Object _this, Warning warning) {
    return warning.getValue();
  }
}

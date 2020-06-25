package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;

@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.")
public class GetStateNode extends Node {
  Object execute(@MonadicState Object state, Object _this) {
    return state;
  }
}

package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
public class PutStateNode extends Node {
  Stateful execute(@MonadicState Object state, Object _this, Object new_state) {
    return new Stateful(new_state, state);
  }
}

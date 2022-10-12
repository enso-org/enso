package org.enso.interpreter.node.expression.builtin.state;

import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "State", name = "put", description = "Updates the value of monadic state.")
@ReportPolymorphism
public abstract class PutStateNode extends Node {
  static PutStateNode build() {
    return PutStateNodeGen.create();
  }

  abstract Object execute(@MonadicState State state, Object key, Object new_state);

  @Specialization
  Object doNothing(State state, Object key, Object new_state) {
    return new_state;
  }
}

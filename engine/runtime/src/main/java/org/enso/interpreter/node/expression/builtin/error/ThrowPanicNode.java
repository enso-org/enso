package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Panic",
    name = "throw",
    description = "Throws a new Panic with given payload.")
public class ThrowPanicNode extends Node {
  Stateful execute(Object _this, Object payload) {
    throw new PanicException(payload, this);
  }
}

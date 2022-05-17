package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Ref;

@BuiltinMethod(type = "Ref", name = "put", description = "Stores a new value in the reference.")
public class PutRefNode extends Node {

  Object execute(Ref _this, Object new_value) {
    Object old = _this.getValue();
    _this.setValue(new_value);
    return old;
  }
}

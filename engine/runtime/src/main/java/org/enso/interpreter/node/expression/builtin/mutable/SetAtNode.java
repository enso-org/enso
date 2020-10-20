package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(
    type = "Array",
    name = "set_at",
    description = "Puts the given element in the given position in the array.")
public class SetAtNode extends Node {

  Object execute(Array _this, long index, Object value) {
    _this.getItems()[(int) index] = value;
    return _this;
  }
}

package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Array",
    name = "to_array",
    description = "Identity on arrays, implemented for protocol completeness.")
public class ToArrayNode extends Node {

  Object execute(Object _this) {
    return _this;
  }
}
